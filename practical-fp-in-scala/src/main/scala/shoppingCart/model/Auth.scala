package shoppingCart.model

import io.estatico.newtype.macros.newtype

import java.util.UUID

trait Auth[F[_]] {
  def findUser(token: JwtToken): F[Option[User]]
  def newUser(username: UserName, password: Password): F[JwtToken]
  def login(username: UserName, password: Password): F[JwtToken]
  def logout(token: JwtToken, username: UserName): F[Unit]
}

@newtype case class UserId(value: UUID)
@newtype case class JwtToken(value: String)

case class User(id: UserId, name: UserName)
