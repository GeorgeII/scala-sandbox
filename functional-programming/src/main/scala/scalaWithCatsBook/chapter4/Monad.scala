package scalaWithCatsBook.chapter4

trait Monad[F[_]] {

  def pure[A](a: A): F[A]

  def flatMap[A, B](value: F[A])(f: A => F[B]): F[B]

  // map can always be achieved by combining Pure and FlatMap
  def map[A, B](value: F[A])(f: A => B): F[B] = {
    flatMap(value)(x => pure(f(x)))
  }

}
