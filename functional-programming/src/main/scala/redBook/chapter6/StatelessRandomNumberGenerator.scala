package redBook.chapter6

trait RNG {

  def nextInt: (Int, RNG)

}

object RNG {

//  type Rand[+A] = RNG => (A, RNG)


  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt

      (n, nextRNG)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (num, gen) = rng.nextInt

    num match {
      case Int.MinValue => (0, gen)
      case _            => (math.abs(num), gen)
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (num, gen) = nonNegativeInt(rng)
    val doubleNum = num.toDouble / (Int.MaxValue.toLong + 1)

    (doubleNum, gen)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (int, gen1)    = rng.nextInt
    val (double, gen2) = RNG.double(gen1)

    ((int, double), gen2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((int, double), gen) = RNG.intDouble(rng)

    ((double, int), gen)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (double1, gen1) = RNG.double(rng)
    val (double2, gen2) = RNG.double(gen1)
    val (double3, gen3) = RNG.double(gen2)

    ((double1, double2, double3), gen3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    (0 until count).foldLeft((List.empty[Int], rng)) { case ((list, gen), _) =>
      val (int, nextGen) = gen.nextInt
      (list :+ int, nextGen)
    }
  }

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def doubleByMap: Rand[Double] = {
    map(nonNegativeInt)(x => x.toDouble / (Int.MaxValue.toLong + 1))
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)

      (f(a, b), rng3)
    }
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  // Even though I came up with the idea to use foldRight, I couldn't finish it. Had to lookup the answer.
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List.empty[A])) { (el, acc) =>
      map2(el, acc)(_ :: _)
    }
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  }

  // map can always be achieved by combining 'flatMap' and 'pure'.
  def mapByFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(x => unit(f(x)))

  def map2ByFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(x => flatMap(rb)(y => unit(f(x, y))))

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)



  // or more general:
  type State[S,+A] = S => (A,S)
  type Rand[A] = State[RNG, A]




}


object StatelessRandomNumberGenerator {


  def main(args: Array[String]): Unit = {
    val rng = RNG.SimpleRNG(42)

    val (n1, rng2) = rng.nextInt
    val (n2, rng3) = rng2.nextInt

    println(n1)
    println(n2)

    println(RNG.nonNegativeInt(rng))
    println(RNG.nonNegativeInt(rng2))
    println(RNG.double(rng))
    println(RNG.double(rng2))

    println(RNG.intDouble(rng))
    println(RNG.doubleInt(rng))
    println(RNG.double3(rng2))

    println(RNG.ints(10)(rng))
    println(RNG.ints(10)(rng2))

    println(RNG.doubleByMap(rng2))


    val int: RNG.Rand[Int] = _.nextInt
    val double: RNG.Rand[Double] = rng => RNG.double(rng)

    val randIntDouble: RNG.Rand[(Int, Double)] =
      RNG.both(int, double)

    val randDoubleInt: RNG.Rand[(Double, Int)] =
      RNG.both(double, int)

    println(randIntDouble(rng))
    println(randDoubleInt(rng))
    println(randIntDouble(rng2))
    println(randDoubleInt(rng2))


  }
}
