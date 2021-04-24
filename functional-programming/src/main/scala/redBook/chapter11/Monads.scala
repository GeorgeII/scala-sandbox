package redBook.chapter11

import scala.{Option => _, Either => _, _}
import redBook.chapter4.{ Some, None, Option }

import redBook.chapter5.{ Stream, Cons, Empty }

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

  def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] = {
    lma.foldRight(unit(List.empty[A]))((el, acc) => map2(el, acc)(_ :: _))
  }

  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] = {
    la.foldRight(unit(List.empty[B]))((el, acc) => map2(f(el), acc)(_ :: _))
  }

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
    sequence(List.fill(n)(ma))
  }

  def product[A,B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    ms.foldRight(unit(List.empty[A])) { (el, acc) =>
      map2(f(el), acc) { (bool, acc) =>
        if(bool) el :: acc
        else acc
      }
    }
  }

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = {
    a: A => flatMap(f(a))(g)
  }

  def join[A](mma: F[F[A]]): F[A] = {
//    flatMap(mma)(ma => flatMap(ma)(a => unit(a)))
    // or
    flatMap(mma)(identity)
  }
}


object Monads {

  def main(args: Array[String]): Unit = {
    // monad instances
    val optionMonad = new Monad[Option] {
      override def unit[A](a: => A): Option[A] = {
        if (a != null) Some(a)
        else None
      }

      override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] =
        ma match {
          case Some(value) => f(value)
          case None        => None
        }
    }

    val streamMonad = new Monad[Stream] {
      override def unit[A](a: => A): Stream[A] =
        a match {
          case () => Empty
          case _  => Cons(() => a, () => Empty)
        }

      override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = {
        ma.flatMap(f)
      }

    }

    val listMonad = new Monad[List] {
      override def unit[A](a: => A): List[A] = {
        List(a)
      }

      override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = {
        ma.flatMap(f)
      }
    }

  }
}
