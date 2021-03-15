package scalaWithCatsBook.chapter4

object Main {

  def main(args: Array[String]): Unit = {

    import cats.Id

    // pure, flatMap, and map for Id.
    // The trick is that Id[A] is just an alias for A, thus we can simply operate with values themselves not having
    // higher kinded type declared.
    def pure[A](value: A): Id[A] = {
      value
    }

    def flatMap[A, B](value: Id[A])(f: A => Id[B]): Id[B] = {
      f(value)
    }

    def map[A, B](value: Id[A])(f: A => B): Id[B] = {
      f(value)
    }

    println(flatMap(555)(_ + 100))
    println(flatMap(555)(_.toDouble * 10))
    println(map(555)(_.toDouble * 10))





  }
}
