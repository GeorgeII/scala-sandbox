package scalaWithCatsBook.chapter4

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

import cats.data.Writer
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.writer._

object WriterMonad {

  type Logged[A] = Writer[Vector[String], A]

  def slowly[A](body: => A): A =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Logged[Int] =
    for {
      ans <- if(n == 0) {
        1.pure[Logged]
      } else {
        slowly(factorial(n - 1).map(_ * n))
      }
      _ <- Vector(s"fact $n $ans").tell
    } yield ans



  def main(args: Array[String]): Unit = {

    val (log, res) = factorial(5).run
    println(log, res)

    val futureRes = Await.result(Future.sequence(List(
      Future(factorial(7)),
      Future(factorial(7))
    )), 5.seconds)

    println(futureRes.map(_.written))
  }

}
