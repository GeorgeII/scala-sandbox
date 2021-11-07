package shoppingCart.model

import cats.data.NonEmptyList
import io.estatico.newtype.macros.newtype
import squants.market.Money

import java.util.UUID

trait Orders[F[_]] {
  def get(
      userId: UserId,
      orderId: OrderId
  ): F[List[Order]]
  def findBy(userId: UserId): F[List[Order]]
  def create(
      userId: UserId,
      paymentId: PaymentId,
      items: NonEmptyList[CartItem],
      total: Money
  ): F[OrderId]
}

@newtype case class OrderId(uuid: UUID)
@newtype case class PaymentId(uuid: UUID)

case class Order(
    id: OrderId,
    pid: PaymentId,
    items: Map[ItemId, Quantity],
    total: Money
)
