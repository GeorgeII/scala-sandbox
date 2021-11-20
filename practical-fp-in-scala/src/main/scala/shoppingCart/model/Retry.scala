package shoppingCart.model

import cats.effect.Temporal
import cats.syntax.show._
import cats.implicits.catsSyntaxSemigroup
import derevo.derive
import derevo.cats.show
import org.typelevel.log4cats.Logger
import retry.RetryDetails.{ GivingUp, WillDelayAndRetry }
import retry.RetryPolicies._
import retry.{ RetryDetails, RetryPolicy, retryingOnAllErrors }

import scala.concurrent.duration.DurationInt

trait Retry[F[_]] {
  def retry[A](
      policy: RetryPolicy[F],
      retriable: Retriable
  )(fa: F[A]): F[A]
}

object Retry {
  def apply[F[_]: Retry]: Retry[F] = implicitly

  implicit def forLoggerTemporal[F[_]: Logger: Temporal]: Retry[F] =
    new Retry[F] {
      def retry[A](
          policy: RetryPolicy[F],
          retriable: Retriable
      )(fa: F[A]): F[A] = {
        val retryPolicy = limitRetries[F](3) |+| exponentialBackoff[F](10.milliseconds)

        def onError(
            e: Throwable,
            details: RetryDetails
        ): F[Unit] =
          details match {
            case WillDelayAndRetry(_, retriesSoFar, _) =>
              Logger[F].error(
                s"Failed on ${retriable.show}. We retried $retriesSoFar times."
              )
            case GivingUp(totalRetries, _) =>
              Logger[F].error(
                s"Giving up on ${retriable.show} after $totalRetries retries."
              )
          }
        retryingOnAllErrors[A](policy, onError)(fa)
      }
    }
}

@derive(show)
sealed trait Retriable

object Retriable {
  final case object Orders   extends Retriable
  final case object Payments extends Retriable
}
