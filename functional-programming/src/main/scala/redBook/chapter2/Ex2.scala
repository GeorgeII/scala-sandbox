package redBook.chapter2

import scala.annotation.tailrec

object Ex2 {

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {

    @tailrec
    def helper[A](n: Int, as: Array[A], ordered: (A,A) => Boolean): Boolean = {
      if (n >= as.length - 1) true
      else if (ordered(as(n), as(n + 1))) helper(n + 1, as, ordered)
      else false
    }

    helper(0, as, ordered)
  }

  def main(args: Array[String]): Unit = {
    val sorted = Array(1, 1, 2, 3, 4)
    val unsorted = Array(-1, 3, 2, 5)

    def compareInts(x: Int, y: Int): Boolean = x <= y

    println(isSorted(sorted, compareInts))
    println(isSorted(unsorted, compareInts))
  }
}
