package scalaWithCatsBook.chapter4

import cats.MonadError

// provide 'pure' and 'raiseError' methods respectively.
import cats.syntax.applicative._
import cats.syntax.applicativeError._

object ValidateAdult {

  def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable]): F[Int] = age match {
    case x if x >= 18 => x.pure[F]
    case _ => new RuntimeException("Too young!").raiseError[F, Int]
  }

}
