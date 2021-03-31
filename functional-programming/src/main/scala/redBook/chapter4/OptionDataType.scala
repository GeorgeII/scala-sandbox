package redBook.chapter4

// to not mess with the built-in Option and Either.
import scala.{Option => _, Either => _, _}


// This is how I implemented it myself. But when I peeked the answer, the methods were implemented in the trait itself.
// I tried to recreate it (see the next comment section).

//sealed trait Option[+A] {
//  def map[B](f: A => B): Option[B]
//  def flatMap[B](f: A => Option[B]): Option[B]
//  def getOrElse[B >: A](default: => B): B
//  def orElse[B >: A](ob: => Option[B]): Option[B]
//  def filter(f: A => Boolean): Option[A]
//}
//
//object Option {
//  def apply[A](a: A): Option[A] = if (a == null) None else Some(a)
//}
//
//case class Some[+A](get: A) extends Option[A] {
//
//  def map[B](f: A => B): Option[B] = Some(f(get))
//
//  override def flatMap[B](f: A => Option[B]): Option[B] = f(get)
//
//  override def getOrElse[B >: A](default: => B): B = get
//
//  override def orElse[B >: A](ob: => Option[B]): Option[B] = Some(get)
//
//  override def filter(f: A => Boolean): Option[A] = if (f(get)) Some(get) else None
//}
//
//
//case object None extends Option[Nothing] {
//  override def map[B](f: Nothing => B): Option[B] = None
//
//  override def flatMap[B](f: Nothing => Option[B]): Option[B] = None
//
//  override def getOrElse[B >: Nothing](default: => B): B = default
//
//  override def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob
//
//  override def filter(f: Nothing => Boolean): Option[Nothing] = None
//}



// this implementation was inspired by the answer (even though, I primarily used pattern matching, the same behavior of
// almost every function can be achieved without it).
sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] =
    this match {
      case Some(v) => Some(f(v))
      case None    => None
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this match {
      case Some(v) => f(v)
      case None    => None
    }

  def getOrElse[B >: A](default: => B): B =
    this match {
      case Some(v) => v
      case None    => default
    }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this match {
      case Some(v) => Some(v)
      case None    => ob
    }

  def filter(f: A => Boolean): Option[A] =
    this match {
      case Some(v) if f(v) => Some(v)
      case None            => None
    }

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]



object OptionDataType {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // tbh, this seems like an overkill. The restriction to use flatMap was strange. In the answer they made it one-liner.
  def variance(xs: Seq[Double]): Option[Double] = {
    val meanOfSeq = mean(xs)

    val seqOfSquares = meanOfSeq.flatMap { m => Some(
                         xs.map { x =>
                           math.pow(x - m, 2.0)
                         })
                       }

    seqOfSquares.flatMap(squares => mean(squares))
  }

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(outer => b.map(inner => f(outer, inner)))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    if (a.contains(None)) None
    else
      a.foldLeft(Some(List.empty[A]): Option[List[A]])((accOp, op) => op.flatMap(el => accOp.map(acc => el :: acc)))
      .map(_.reverse)
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldLeft(Some(List.empty[B]): Option[List[B]])((accOp, el) => accOp.flatMap(acc => f(el).map(_ :: acc)))
      .map(_.reverse)
  }

  def sequenceFromTraverse[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(x => x)
  }


  def main(args: Array[String]): Unit = {
    val x = Some(5)
    println(x)
    println(x.flatMap(value => Some(value + value)))


    println("mean and variance:")
    val seq = List(1.0, 2.0, 3.0, 4.0, 5.0)
    println(mean(seq))
    println(variance(seq))


    println("sequence:")
    val l = List(Some(3), Some(5), Some(8), Some(12))
    println(sequence(l))
    val lNone = l ::: List[Option[Int]](None, Some(25))
    println(sequence(lNone))

    println("traverse:")
    println(traverse(seq)(x => Some(x)))
    println(traverse(seq)(_ => None))

    println("sequenceFromTraverse:")
    println(sequenceFromTraverse(l))
    println(sequenceFromTraverse(lNone))

  }
}
