package redBook.chapter3

import scala.annotation.tailrec

object ListFunctions {

  def tail[A](as: List[A]): List[A] = {
    as match {
      case Nil     => Nil
      case x :: xs => xs
    }
  }

  def setHead[A](as: List[A], head: A): List[A] = {
    as match {
      case Nil     => List(head)
      case x :: xs => head :: xs
    }
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    n match {
      case 0              => l
      case x if l.isEmpty => l
      case _              => drop(l.tail, n - 1)
    }
  }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil             => Nil
      case x :: xs if f(x) => dropWhile(xs, f)
      case _               => l
    }
  }

  def init[A](l: List[A]): List[A] = {
    @tailrec
    def loop(l: List[A], acc: List[A]): List[A] = {
      l match {
        case Nil      => Nil
        case x :: Nil => acc.reverse
        case x :: xs  => loop(xs, x :: acc)
      }
    }

    loop(l, List.empty)
  }

  def length[A](as: List[A]): Int = {
    as.foldRight(0)((_, len) => len + 1)
  }

  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil     => z
      case x :: xs => foldLeft(xs, f(z, x))(f)
    }
  }

  def sum(as: List[Int]): Int = {
    as.foldLeft(0)(_ + _)
  }

  def product(as: List[Int]): Int = {
    as.foldLeft(1)(_ * _)
  }

  def lengthFoldLeft[A](as: List[A]): Int = {
    as.foldLeft(0)((len, _) => len + 1)
  }

  def reverse[A](as: List[A]): List[A] = {
    as.foldLeft(List.empty[A])((acc, el) => el :: acc)
  }

  def append[A](a: A, as: List[A]): List[A] = {
    as.foldLeft(List(a))((acc, el) => el :: acc).reverse
  }

  def concatAll[A](as: List[List[A]]): List[A] = {
    as.foldRight(List.empty[A]) {
      (innerList, acc) => innerList.foldRight(acc) {
        (elementOfInnerList, accOfInnerList) => elementOfInnerList :: accOfInnerList
      }
    }
  }

  def plusOne(as: List[Int]): List[Int] = {
    as.foldRight(List.empty[Int])((el, acc) => (el + 1) :: acc)
  }

  def doubleToString(as: List[Double]): List[String] = {
    as.foldRight(List.empty[String])((el, acc) => el.toString :: acc)
  }

  def map[A,B](as: List[A])(f: A => B): List[B] = {
    as.foldRight(List.empty[B])((el, acc) => f(el) :: acc)
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    as.foldRight(List.empty[A]) {
      (el, acc) =>
        if (f(el)) el :: acc
        else acc
    }
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    concatAll(as.map(f))
  }

  def filterFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    as.flatMap(el => {
      if (f(el)) List(el)
      else List.empty[A]
    })
  }

  // Even though it works, it seems to be an overkill because this is not very efficient.
  def addCorresponding(l1: List[Int], l2: List[Int]): List[Int] = {
    val minLen = l1.length min l2.length
    (0 until minLen).map(idx => l1(idx) + l2(idx)).toList
  }

  // Also inefficient like the previous one.
  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = {
    val minLen = l1.length min l2.length
    (0 until minLen).map(idx => f(l1(idx), l2(idx))).toList
  }

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    if (sub.isEmpty)             return true
    if (sup.length < sub.length) return false

    @tailrec
    def strictConsequentMatch(l1: List[A], l2: List[A]): Boolean = {
      l2 match {
        case Nil                     => true
        case x :: xs if l1.head == x => strictConsequentMatch(l1.tail, xs)
        case _                       => false
      }
    }

    sup match {
      case Nil                      => sub.isEmpty
      case x :: xs if x == sub.head => strictConsequentMatch(xs, sub.tail)
      case x :: xs                  => hasSubsequence(xs, sub)
    }
  }


  def main(args: Array[String]): Unit = {

    println("Test 'tail' function")
    println(tail(List(1, 2, 3)))
    println(tail(List(2, 3)))
    println(tail(List(3)))
    println(tail(List()))

    println("Test 'setHead' function")
    println(setHead(List(1, 2, 3), 0))
    println(setHead(List(3), 0))
    println(setHead(List(), 0))

    println("Test 'drop' function")
    println(drop(List(1, 2, 3), 0))
    println(drop(List(1, 2, 3), 1))
    println(drop(List(1, 2, 3), 2))
    println(drop(List(1, 2, 3), 3))
    println(drop(List(3), 1))
    println(drop(List(), 2))

    println("Test 'dropWhile' function")
    println(dropWhile(List(1, 2, 3, 4, 5, 6), (x: Int) => x < 4))
    println(dropWhile(List(1, 2, 3, 4, 5, 6), (x: Int) => x < 8))
    println(dropWhile(List(1), (x: Int) => x < 4))
    println(dropWhile(List(1), (x: Int) => x > 4))
    println(dropWhile(List(), (x: Int) => x < 4))

    println("Test 'init' function")
    println(init(List(1, 2, 3, 4)))
    println(init(List(1, 2)))
    println(init(List(1)))
    println(init(List()))

    println("Test 'length' function")
    println(length(List(1, 2, 3, 4)))
    println(length(List(1, 2, 3, 4, 5)))
    println(length(List(1)))
    println(length(List()))

    println("Test 'foldLeft' function")
    println(foldLeft(List(1, 2, 3, 4), 0)(_ + _))
    println(foldLeft(List(1, 2, 3, 4, 5), 1)(_ * _))
    println(foldLeft(List(1), "a")(_ + _.toString))
    println(foldLeft(List[Int](), 15)(_ + _))

    println("Test 'lengthFoldLeft' function")
    println(lengthFoldLeft(List(1, 2, 3, 4)))

    println("Test 'reverse' function")
    println(reverse(List(1, 2, 3, 4)))

    println("Test 'append' function")
    println(append(0, List(1, 2, 3, 4)))

    println("Test 'concatAll' function")
    println(concatAll(List(List(1, 2), List(3, 4), List(5, 6))))

    println("Test 'plusOne' function")
    println(plusOne(List(1, 2, 3, 4, 5)))

    println("Test 'doubleToString' function")
    println(doubleToString(List(1, 2, 3, 4, 5)))

    println("Test 'map' function")
    println(map(List(1, 2, 3, 4, 5))(_ + 5))

    println("Test 'filter' function")
    println(filter(List(1, 2, 3, 4, 5, 6, 7, 8))(_ % 2 == 0))

    println("Test 'flatMap' function")
    println(flatMap(List(1,2,3))(i => List(i,i)))

    println("Test 'filterFlatMap' function")
    println(filterFlatMap(List(1, 2, 3, 4, 5, 6, 7, 8))(_ % 2 == 0))

    println("Test 'addCorresponding' function")
    println(addCorresponding(List(1, 2, 3), List(10, 11, 12)))
    println(addCorresponding(List(1, 2, 3, 4), List(10, 11, 12)))
    println(addCorresponding(List(1, 2, 3), List(10, 11, 12, 13)))

    println("Test 'zipWith' function")
    println(zipWith(List(1, 2, 3), List(10, 11, 12))(_ * _))
    println(zipWith(List(1, 2, 3, 4), List("a", "b", "c"))(List(_, _)))

    println("Test 'hasSubsequence' function")
    println(hasSubsequence(List(1,2,3,4), List(1,2)))
    println(hasSubsequence(List(1,2,3,4), List(2,3)))
    println(hasSubsequence(List(1,2,3,4), List(4)))
    println(hasSubsequence(List(1,2,3,4), List()))
    println(hasSubsequence(List(1,2), List(1,2,3,4)))
    println(hasSubsequence(List(1,2,3,4), List(2,4)))
  }
}
