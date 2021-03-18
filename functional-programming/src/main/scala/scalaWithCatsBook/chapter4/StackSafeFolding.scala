package scalaWithCatsBook.chapter4

import cats.Eval

object StackSafeFolding {

  def foldRight[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(fn(head, foldRight(tail, acc)(fn)))
      case Nil =>
        acc
    }


  def main(args: Array[String]): Unit = {

    val list = (1 to 3000000).toList

    // handling Eval in required places to use foldRight function above.
    val sum = StackSafeFolding.foldRight(list, Eval.now(0L))((x, y) => y.map(wrappedValue => x + wrappedValue))

    println(sum.value)

  }
}
