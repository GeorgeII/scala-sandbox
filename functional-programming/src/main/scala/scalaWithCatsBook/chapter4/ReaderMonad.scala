package scalaWithCatsBook.chapter4

import cats.data.Reader
import cats.syntax.applicative._

object ReaderMonad {

  final case class Db(
    usernames: Map[Int, String],
    passwords: Map[String, String]
  )

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] = {
    Reader(db => {
      if (db.usernames.contains(userId)) Option(db.usernames(userId)) else None
    })
  }

  def checkPassword(username: String, password: String): DbReader[Boolean] = {
    Reader(db => db.passwords(username).contains(password))
  }

  def checkLogin(userId: Int, password: String): DbReader[Boolean] = {
    for {
      user              <- findUsername(userId)
      isPasswordCorrect <- user.map {username =>
                              checkPassword(username, password)
                           }.getOrElse(false.pure[DbReader])
    } yield isPasswordCorrect
  }


  def main(args: Array[String]): Unit = {

    val users = Map(
      1 -> "dade",
      2 -> "kate",
      3 -> "margo"
    )

    val passwords = Map(
      "dade" -> "zerocool",
      "kate" -> "acidburn",
      "margo" -> "secret"
    )

    val db = Db(users, passwords)

    val check1 = checkLogin(1, "zerocool").run(db)
    val check2 = checkLogin(4, "davinci").run(db)

    println(check1)
    println(check2)
  }
}
