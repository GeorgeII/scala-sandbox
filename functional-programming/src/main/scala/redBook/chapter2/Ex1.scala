package redBook.chapter2

import scala.annotation.tailrec

object Ex1 {

  def fibonacci(n: Int): Int = {

    @tailrec
    def helper(prev: Int, curr: Int, currentStep: Int): Int = {
      if (currentStep == n) prev
      else helper(curr, prev + curr, currentStep + 1)
    }

    // this is a formula to calculate a fibonacci seq for negative n.
    if (n < 0) scala.math.pow(-1, n + 1).toInt * fibonacci(-n)
    // this is how it's typically done for a positive number.
    else helper(0, 1, 0)
  }

  def main(args: Array[String]): Unit = {
    (-11 to 11).foreach(n => println(fibonacci(n)))
  }
}
