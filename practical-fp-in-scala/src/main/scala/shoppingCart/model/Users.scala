package shoppingCart.model

import io.estatico.newtype.macros.newtype

trait Users[F[_]] {
  def find(username: UserName): F[Option[UserWithPassword]]
  def create(
      username: UserName,
      password: EncryptedPassword
  ): F[Unit]
}

@newtype case class UserName(value: String)
@newtype case class Password(value: String)
@newtype case class EncryptedPassword(value: String)

case class UserWithPassword(
    id: UserId,
    name: UserName,
    password: EncryptedPassword
)
