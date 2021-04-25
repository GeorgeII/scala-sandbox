package redBook.chapter12

import scala.{Either => _, Option => _, _}
import redBook.chapter4.{Either, Left, Right}
import redBook.chapter11.Monad

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}

trait Applicative[F[_]] extends Functor[F] {

  def unit[A](a: => A): F[A]

//  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
//
//  def map[A, B](fa: F[A])(f: A => B): F[B] =
//    map2(fa, unit(()))((a, _) => f(a))

  // we can define map and map2 in terms of apply and unit.
  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B]

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  // this one is pretty hard... They used currying. Very smart solution.
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)


  def map3[A,B,C,D](fa: F[A],
                    fb: F[B],
                    fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(map(fa)(f.curried))(fb))(fc)

  def map4[A,B,C,D,E](fa: F[A],
                      fb: F[B],
                      fc: F[C],
                      fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(map(fa)(f.curried))(fb))(fc))(fd)



  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldRight(unit(List.empty[A]))((el, acc) => map2(el, acc)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    unit(List.fill(n)(fa))

  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
    map2(fa, fb)((a, b) => (a, b))
}


sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector())
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]



object ApplicativeFunctors {

  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = {
    new Monad[({type f[x] = Either[E, x]})#f] {
      override def unit[A](a: => A): Either[E, A] = {
        Right(a)
      }

      override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] = {
        ma match {
          case Right(value) => f(value)
          case Left(value)  => Left(value)
        }
      }
    }
  }

//  def applicativeValidation[E]: Applicative[({type f[x] = Validation[E, x]})#f] = {
//    new Applicative[({type f[x] = Validation[E, x]})#f] {
//      override def unit[A](a: => A): Validation[E, A] = {
//        Success(a)
//      }
//
//      override def apply[A, B](fab: Validation[E, A => B])(fa: Validation[E, A]): Validation[E, B] = {
//        (fa, fab) match {
//          case (Success(a), Success(b)) => Success(b(a))
//          case (Success(a), Failure(h, t)) => Failure(a, h +: t)
//          case (Failure(h, t), Success(b)) => Failure(b, h +: t)
//          case (Failure(h1, t1), Failure(h2, t2)) => Failure()
//        }
//      }
//    }
//  }


  def main(args: Array[String]): Unit = {

  }
}
