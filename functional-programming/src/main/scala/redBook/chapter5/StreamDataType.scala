package redBook.chapter5

import redBook.chapter5.Stream.{cons, fibs, from, unfold}

import scala.annotation.tailrec

sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = {
    @tailrec
    def helper(accList: List[A], stream: Stream[A]): List[A] = {
      stream match {
        case Empty => accList.reverse
        case Cons(h, t) => helper(h() :: accList, t())
      }
    }

    helper(List.empty[A], this)
  }

  // this was an interesting one. Had to lookup at the solution because I tried to write it with a helper function.
  // Though, the stack can be overflowed.
  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
      case Cons(h, t) if n <= 1 => cons(h(), Empty)
      case Empty                => Empty
    }
  }

  def drop(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n >= 1 => t().drop(n - 1)
      case _                    => this
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _                    => Empty
    }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(h,t) => f(h(), t().foldRight(z)(f))
        case _         => z
    }

  def forAll(p: A => Boolean): Boolean = {
    this match {
      case Cons(h, t) => p(h()) && t().forAll(p)
      case Empty      => true
    }
  }

  def takeWhileByFold(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((el, acc) => {
      if (p(el)) cons(el, acc)
      else acc
    })
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(Stream.empty[B])((el, acc) => cons(f(el), acc))
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A]) { (el, acc) =>
      if (p(el)) cons(el, acc)
      else acc
    }
  }

  def prepend[B >: A](b: => Stream[B]): Stream[B] = {
    b.foldRight(this.asInstanceOf[Stream[B]])((el, acc) => cons(el, acc))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Stream.empty[B])((el, acc) => acc.prepend(f(el)))
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def mapByUnfold[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Cons(h, t) => Option(f(h()), t())
      case Empty      => None
    }
  }

  def takeByUnfold(n: Int): Stream[A] = {
    unfold((0, this)) {
      case (step, Cons(h, t)) if step < n => Option(h(), (step + 1, t()))
      case _                              => None
    }
  }

  def takeWhileByUnfold(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if p(h()) => Option(h(), t())
      case _                    => None
    }
  }

  def zipWithByUnfold[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Option(f(h1(), h2()), (t1(), t2()))
      case _                            => None
    }
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = {
    unfold(this, s2) {
      case (Cons(h1, t1), Cons(h2, t2)) => Option((Option(h1()), Option(h2())), (t1(), t2()))
      case (Cons(h1, t1), Empty)        => Option((Option(h1()), None), (t1(), Empty))
      case (Empty, Cons(h2, t2))        => Option((None, Option(h2())), (Empty, t2()))
      case _                            => None
    }
  }

  // absolutely correct version but previously written functions are not used.
//  def startsWith[B >: A](s: Stream[B]): Boolean = {
//    (this, s) match {
//      case (Cons(h1, t1), Cons(h2, t2)) if h1() == h2() => t1().startsWith(t2())
//      case (_, Empty)                                   => true
//      case _                                            => false
//    }
//  }

  // though, I accidentally peeked a beginning of the answer a little bit, the solution seems pretty easy after
  // you get 'zipAll' concept and remember about takeWhile function :)
  def startsWith[B >: A](s: Stream[B]): Boolean = {
    this.zipAll(s).takeWhile(_._2.nonEmpty).forAll {case (x, y) => x == y}
  }

  def tails: Stream[Stream[A]] = {
    unfold(Option(this)) {
      case Some(stream @ Cons(h, t)) => Option(stream, Option(t()))
      case Some(Empty)               => Option(Empty, None)
      case None                      => None
    }
  }

  def hasSubsequence[B >: A](s: Stream[B]): Boolean =
    tails exists (_ startsWith s)
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl

    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = {
    lazy val constants: Stream[A] = Stream.cons(a, constants)
    constants
  }

  def from(n: Int): Stream[Int] = {
    lazy val growingStream: Stream[Int] = Stream.cons(n, from(n + 1))
    growingStream
  }

  def fibs(): Stream[Int] = {
    def helper(prev: Int, curr: Int): Stream[Int] = {
      val next = prev + curr
      lazy val seq = Stream.cons(next, helper(curr, next))
      seq
    }

    lazy val fibonacciStream = Stream.cons(0, Stream.cons(1, helper(0, 1)))
    fibonacciStream
  }

  // my version. Throws an exception if None was generated by f.
//  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
//    lazy val stream = f(z).map(valAndState => Stream.cons(valAndState._1, unfold(valAndState._2)(f)))
//
//    stream
//      .getOrElse(throw new Exception("Out of stream boundaries! Unfold cannot generate next elements in the sequence!"))
//  }

  // unfold function from the solution. It smartly cuts Stream whenever it reaches None by f.
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None        => empty
    }

  def fromByUnfold(n: Int): Stream[Int] = {
    unfold(n)(x => Option(x, x + 1))
  }

  def fibsByUnfold(): Stream[Int] = {
    unfold((0, 1))(x => Option(x._1, (x._2, x._1 + x._2)))
  }

  def constantByUnfold[A](a: A): Stream[A] = {
    unfold(a)(x => Option(x, x))
  }

  def onesByUnfold(): Stream[Int] =
    constantByUnfold(1)

}



object StreamDataType {

  def main(args: Array[String]): Unit = {

    println("Stream to List")
    val s = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    println(s)
    println(s.toList)

    println("take:")
    println(s.take(5))
    println(s.take(5).toList)
    println(s.take(7).toList)

    println("drop:")
    println(s.drop(5))
    println(s.drop(5).toList)
    println(s.drop(7).toList)

    println("takeWhile:")
    println(s.takeWhile(_ < 5))
    println(s.takeWhile(_ < 5).toList)
    println(s.takeWhile(_ < 7).toList)

    println("forAll:")
    println(s.forAll(_ < 5))
    println(s.forAll(_ < 10))
    println(s.forAll(_ < 11))

    println("takeWhileByFold:")
    println(s.takeWhileByFold(_ < 5))
    println(s.takeWhileByFold(_ < 5).toList)
    println(s.takeWhileByFold(_ < 7).toList)

    println("map:")
    println(s.map(_.toDouble).toList)
    println(s.map(_ + 10).toList)

    println("filter:")
    println(s.filter(_ % 2 == 0).toList)
    println(s.filter(_ < 6).toList)

    println("prepend:")
    println(s.prepend(Stream(-2, -1)).toList)
    println(s.prepend(Stream(15, 16, 17)).toList)
    println(s.prepend(Stream(-2, -1)).prepend(Stream(-4, -3)).toList)

    println("flatMap:")
    println(s.flatMap(x => Stream(x.toDouble)).toList)
    println(s.flatMap(x => Stream(x + 10)).toList)


    // the example in the book that shows Streams are "first-class loops", i.e. evaluated without instantiating
    // intermediate Streams.
    println(Stream(1,2,3,4).map(_ + 10).filter(_ % 2 == 0).toList)


    // infinite stream
    println("infinite stream of ones:")
    lazy val ones: Stream[Int] = Stream.cons(1, ones)
    println(ones.take(5).toList)
    println(ones.exists(_ % 2 != 0))
    println(ones.map(_ + 1).exists(_ % 2 == 0))
    println(ones.takeWhile(_ == 1))
    println(ones.forAll(_ != 1))


    println("from (growing stream of ints):")
    lazy val growingStream = from(22)
    println(growingStream.take(9).toList)

    println("fibonacci:")
    lazy val fib = fibs()
    println(fib.take(15).toList)

    println("unfold:")
    lazy val unfoldStream = Stream.unfold(2)({
      case x if x < 320 => Option(x + 3, x + 10)
      case _            => None
    })
    println(unfoldStream.take(29).toList)

    println("fromByUnfold:")
    println(Stream.fromByUnfold(22).take(9).toList)

    println("fibsByUnfold:")
    println(Stream.fibsByUnfold().take(15).toList)

    println("constantByUnfold:")
    println(Stream.constantByUnfold(33).take(15).toList)

    println("onesByUnfold:")
    println(Stream.onesByUnfold().take(15).toList)

    println("mapByUnfold:")
    println(s.mapByUnfold(_.toDouble).toList)
    println(s.mapByUnfold(_ + 10).toList)

    println("takeByUnfold:")
    println(s.takeByUnfold(5))
    println(s.takeByUnfold(5).toList)
    println(s.takeByUnfold(7).toList)

    println("takeWhileByUnfold:")
    println(s.takeWhileByUnfold(_ < 5))
    println(s.takeWhileByUnfold(_ < 5).toList)
    println(s.takeWhileByUnfold(_ < 7).toList)

    println("zipWithByUnfold")
    println(
      Stream.from(-5)
      .zipWithByUnfold(Stream.constant("str"))(_.toString + _)
      .take(15)
      .toList
    )

    println("zipAll")
    println(
      Stream.from(-5)
        .take(5)
        .zipAll(Stream("a", "b", "c", "d", "e", "f", "g"))
        .take(15)
        .toList
    )

    println("startsWith:")
    println(
      Stream
        .from(0)
        .take(10)
        .startsWith(
          Stream
            .from(0)
            .take(4)
        )
    )
    println(
      Stream
        .from(0)
        .take(10)
        .startsWith(
          Stream
            .from(15)
            .take(4)
        )
    )

    println("tails:")
    println(Stream(1,2,3).tails.toList.map(_.toList))
    println(Stream(1,2,3,4,5).tails.toList.map(_.toList))

    println("hasSubsequence:")
    println(
      Stream
        .from(0)
        .take(15)
        .hasSubsequence(
          Stream
            .from(6)
            .take(6)
        )
    )
    println(
      Stream
        .from(0)
        .take(15)
        .hasSubsequence(
          Stream
            .from(-1)
            .take(6)
        )
    )

  }
}
