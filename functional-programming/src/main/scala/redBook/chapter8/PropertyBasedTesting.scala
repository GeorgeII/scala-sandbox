package redBook.chapter8

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  case class Prop(run: (TestCases, RNG) => Result) {
//    def &&(p: Prop): Prop =
//
//
//    def ||(p: Prop): Prop =

  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

//import Prop._

case class Gen[A](sample: State[RNG,A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(x => f(x).sample))

//  def listOfN(size: Gen[Int]): Gen[List[A]] =
//    size.flatMap(x => Gen.listOfN(x, this))

}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeLessThan(stopExclusive - start)).map(_ + start))

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

//  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
//    unit(List.fill(n)(g.sample))

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



  def main(args: Array[String]): Unit = {

  }
}
