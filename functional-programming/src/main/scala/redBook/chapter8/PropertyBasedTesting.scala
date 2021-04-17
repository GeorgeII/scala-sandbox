package redBook.chapter8

//import redBook.chapter8.RNG.Simple
import redBook.chapter8.State.Rand

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}

trait Prop {
  import Prop._

  def check: Either[(FailedCase, SuccessCount), SuccessCount]
}

case class Gen[A](sample: State[RNG,A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(x => f(x).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(x => Gen.listOfN(x, this))

}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeLessThan(stopExclusive - start)).map(_ + start))

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    unit(List.fill(n)(g.sample))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean flatMap {
      case true  => g1
      case false => g2
    }

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] =
    choose(0, 100) flatMap {
      case x if x <= g1._2 * 100 => g1._1
      case _                     => g2._1
    }



}


object PropertyBasedTesting {



//  def listOf[A](a: Gen[A]): Gen[List[A]]
//
//  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]]
//
//  def forAll[A](a: Gen[A])(f: A => Boolean): Prop


  def main(args: Array[String]): Unit = {

  }
}
