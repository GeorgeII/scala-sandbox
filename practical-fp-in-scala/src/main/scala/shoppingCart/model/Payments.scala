package shoppingCart.model

import squants.market.Money

trait Payments[F[_]] {
  def process(payment: Payment): F[PaymentId]
}

case class Payment(
    id: UserId,
    total: Money,
    card: Card
)
