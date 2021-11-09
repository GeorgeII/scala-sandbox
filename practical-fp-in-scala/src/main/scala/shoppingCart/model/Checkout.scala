package shoppingCart.model

import cats.data.NonEmptyList
import cats.{ Monad, MonadThrow }

final case class Checkout[F[_]: Monad](
    payments: PaymentClient[F],
    cart: ShoppingCart[F],
    orders: Orders[F]
) {

  private def ensureNonEmpty[A](xs: List[A]): F[NonEmptyList[A]] =
    MonadThrow[F].fromOption(
      NonEmptyList.fromList(xs),
      EmptyCartError
    )

  def process(userId: UserId, card: Card): F[OrderId] =
    for {
      c   <- cart.get(userId)
      its <- ensureNonEmpty(c.items)
      pid <- payments.process(Payment(userId, c.total, card))
      oid <- orders.create(userId, pid, its, c.total)
      _   <- cart.delete(userId)
    } yield oid

}
