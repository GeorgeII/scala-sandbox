package shoppingCart.model

import io.estatico.newtype.macros.newtype

import java.util.UUID

trait Brands[F[_]] {
  def findAll: F[List[Brand]]

  def create(name: BrandName): F[BrandId]
}

@newtype case class BrandId(value: UUID)
@newtype case class BrandName(value: String)

case class Brand(uuid: BrandId, name: BrandName)
