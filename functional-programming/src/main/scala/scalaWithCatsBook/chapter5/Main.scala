package scalaWithCatsBook.chapter5

import cats.data.EitherT

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object Main {

  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz"      -> 6,
    "Bumblebee" -> 8,
    "Hot Rod"   -> 10
  )

  def getPowerLevel(autobot: String): Response[Int] =
    powerLevels.get(autobot) match {
      case Some(value) => EitherT.right(Future(value))
      case None        => EitherT.left(Future(s"$autobot autobot is unreachable."))
    }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      powerOfAlly1 <- getPowerLevel(ally1)
      powerOfAlly2 <- getPowerLevel(ally2)
    } yield powerOfAlly1 + powerOfAlly2 > 15

  def tacticalReport(ally1: String, ally2: String): String = {
    val canTheyMove = canSpecialMove(ally1, ally2).value

    Await.result(canTheyMove, 2.seconds) match {
      case Right(true)  => s"$ally1 and $ally2 are ready to roll out!"
      case Right(false) => s"$ally1 and $ally2 need a recharge."
      case Left(error)  => s"Comms error: $error"
    }
  }


  def main(args: Array[String]): Unit = {

  val res1 = getPowerLevel("Bumblebee")
    val res2 = getPowerLevel("John")
    val res3 = getPowerLevel("Jazz")

    println(res1)
    println(res2)
    println(res3)

    println(canSpecialMove("Bumblebee", "Hot Rod"))


    println(tacticalReport("Jazz", "Bumblebee"))
    // res13: String = "Jazz and Bumblebee need a recharge."
    println(tacticalReport("Bumblebee", "Hot Rod"))
    // res14: String = "Bumblebee and Hot Rod are ready to roll out!"
    println(tacticalReport("Jazz", "Ironhide"))
    // res15: String = "Comms error: Ironhide unreachable"

  }
}
