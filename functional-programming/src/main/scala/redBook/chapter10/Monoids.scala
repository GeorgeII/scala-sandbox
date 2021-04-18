package redBook.chapter10

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoids {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }

  val intMultiplication = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 1
  }

  val booleanOr = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = false
  }

  val booleanAnd = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a1 andThen a2
    override def zero: A => A = identity
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.map(f).foldLeft(m.zero)(m.op)

  // or even without a map function inside
//  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B =
//    as.foldLeft(m.zero)((acc, el) => m.op(acc, f(el)))

  def foldRight[A, B](as: List[A])(z: B)(op: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(op.curried)(z)


  // foldLeft from the answer cause it requires dual which I missed to write.
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)


  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.length == 1)
      return f(v(0))

    val middle = v.length / 2
    val (firstHalf, secondHalf)  = v.splitAt(middle)

    m.op(foldMapV(firstHalf, m)(f), foldMapV(secondHalf, m)(f))
  }


  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  // my implementation is quite bad...
//  val wcMonoid: Monoid[WC] = new Monoid[WC] {
//    override def op(a1: WC, a2: WC): WC = a1 match {
//      case Stub(a1Chars) => a2 match {
//        case Stub(a2Chars)             => Stub(a1Chars + a2Chars)
//        case Part(lStub, words, rStub) => Part("", words + 1, rStub)
//      }
//
//      case Part(a1lStub, a1words, a1rStub) => a2 match {
//        case Stub(chars)                     => Part(a1lStub, a1words, a1rStub + chars)
//        case Part(a2lStub, a2words, a2rStub) => Part(a1lStub, a1words + a2words + 1, a2rStub)
//      }
//    }
//
//    override def zero: WC = Part("", 0, "")
//  }
//
//  def countWords(string: String): Int = {
//    if (string.isEmpty)
//      return 0
//
//    if (string.length == 1)
//      return 1
//
//    val (left, right) = string.splitAt(string.length / 2)
//
//    wcMonoid.op(countWords(left), countWords(right))
//  }

  trait Foldable[F[_]] {
    def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B
    def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B
    def foldMap[A,B](as: F[A], mb: Monoid[B])(f: A => B): B
    def concatenate[A](as: F[A])(m: Monoid[A]): A =
      foldLeft(as)(m.zero)(m.op)
  }

  val foldableList = new Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      this.foldMap(as, endoMonoid[B])(f.curried)(z)

    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      this.foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

    override def foldMap[A, B](as: List[A], mb: Monoid[B])(f: A => B): B =
      as.map(f).foldLeft(mb.zero)(mb.op)
  }


  def main(args: Array[String]): Unit = {
    println(foldableList.foldLeft(List(1, 2, 3, 4, 5))(0)(_ + _))
  }
}
