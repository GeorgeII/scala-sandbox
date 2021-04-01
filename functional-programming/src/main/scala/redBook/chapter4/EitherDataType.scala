package redBook.chapter4

// to not mess with the built-in Option and Either.
import scala.{Option => _, Either => _, _}

trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Right(value) => Right(f(value))
      case Left(value)  => Left(value)
    }
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Right(value) => f(value)
      case Left(value)  => Left(value)
    }
  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Right(value) => Right(value)
      case Left(value)  => b
    }
  }

  // how I implemented it.
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    this match {
      case Right(value) => b.flatMap(bb => Right(f(value, bb)))
      case Left(value)  => Left(value)
    }
  }

  // and this is their version of map2. Looks nicer.
//  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
//    for {
//      a  <- this
//      bb <- b
//    } yield f(a, bb)
//  }
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]


object EitherDataType {

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    es.foldRight[Either[E, List[A]]](Right(List.empty[A])) { (elEither, accEither) =>
      elEither.flatMap(el => accEither.map(acc => el :: acc))
    }
  }

  def traverse[E, A, B](as: List[A])(
                        f: A => Either[E, B]): Either[E, List[B]] = {
    as.foldRight[Either[E, List[B]]](Right(List.empty[B])) { (el, accEither) =>
      accEither.flatMap(acc => f(el).map(res => res :: acc))
    }
  }


  def main(args: Array[String]): Unit = {

    val x = Right(5)
    val y = Left("error")
    println(x)
    println(x.map(_.toDouble))
    println(y)
    println(y.flatMap(_ => Right(5)))
    println(x.map2(Right(7))(_ + _))

    val listOfEith = List(Right(2), Right(5), Left("Err"), Right(10), Left("Another Err"))
    println(sequence(listOfEith))

  }
}
