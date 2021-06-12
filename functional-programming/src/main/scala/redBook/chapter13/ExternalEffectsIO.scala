package redBook.chapter13

import redBook.chapter11.Monad

import scala.io.StdIn.readLine


trait IO[A] { self =>
  def run(): A

  def ++[B](io: IO[B]): IO[B] = new IO[B] {
    override def run(): B = {
      self.run()
      io.run()
    }
  }

  def map[B](f: A => B): IO[B] = {
    new IO[B] { def run(): B = f(self.run()) }
  }

  def flatMap[B](f: A => IO[B]): IO[B] = {
    new IO[B] { def run(): B = f(self.run()).run() }
  }
}

object IO extends Monad[IO] {
  def unit[A](a: => A): IO[A] = new IO[A] { def run(): A = a }

  def flatMap[A,B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f

  def apply[A](a: => A): IO[A] = unit(a)
}



object ExternalEffectsIO {

  case class Player(name: String, score: Int)

  def winner(p1: Player, p2: Player): Option[Player] =
    if (p1.score > p2.score) Some(p1)
    else if (p1.score < p2.score) Some(p2)
    else None

  def winnerMsg(p: Option[Player]): String = p map {
    case Player(name, _) => s"$name is the winner!"
  } getOrElse "It's a draw."

//  def contest(p1: Player, p2: Player): Unit =
//    println(winnerMsg(winner(p1, p2)))

  def contest(p1: Player, p2: Player): IO[Unit] =
    PrintLine(winnerMsg(winner(p1, p2)))



  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0/9.0

  def ReadLine: IO[String] = IO { readLine }

  def PrintLine(msg: String): IO[Unit] = IO { println(msg) }

  def converter: IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()




  case class Return[A](a: A) extends TailRec[A]
  case class Suspend[A](resume: () => A) extends TailRec[A]
  case class FlatMap[A,B](sub: TailRec[A],
                          k: A => TailRec[B]) extends TailRec[B]

  sealed trait TailRec[A] {
    def flatMap[B](f: A => TailRec[B]): TailRec[B] =
      FlatMap(this, f)

    def map[B](f: A => B): TailRec[B] =
      flatMap(f andThen (Return(_)))
  }





  def main(args: Array[String]): Unit = {
    converter.run()


    val readInt  = ReadLine.map(_.toInt)
    val printInt = readInt.flatMap(int => PrintLine(int.toString))
    printInt.run()

  }
}
