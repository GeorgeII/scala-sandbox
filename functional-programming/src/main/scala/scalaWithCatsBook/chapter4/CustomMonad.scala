package scalaWithCatsBook.chapter4

import cats.Monad

import scala.annotation.tailrec

object CustomMonad {

  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A])
    extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] =
    Leaf(value)


  // this is what I've done. But in the solution they declared it as an implicit value. Though, my flatMap code was
  // absolutely correct and almost identical to theirs.

//  def flatMap[A, B](value: Tree[A])(f: A => Tree[B]): Tree[B] = {
//    value match {
//      case Branch(left, right) => branch(flatMap(left)(f), flatMap(right)(f))
//      case Leaf(value)         => f(value)
//    }
//  }
//
//  def map[A, B](value: Tree[A])(f: A => B): Tree[B] = {
//    flatMap(value)(x => f(x).pure)
//  }


  def main(args: Array[String]): Unit = {
    val leftBranch = branch(leaf(1), leaf(2))
    val rightBranch = branch(leaf(3), leaf(4))
    val root = branch(leftBranch, rightBranch)

    println(root)

    // this is for the commented methods above.
//    val res = flatMap(root)(node => leaf(node.toDouble))
//    println(res)


    // This is a mix of my code and the code from the solution.

    implicit val treeMonad = new Monad[Tree] {
      override def pure[A](x: A): Tree[A] = Leaf(x)

      override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = {
        fa match {
          case Branch(left, right) => branch(flatMap(left)(f), flatMap(right)(f))
          case Leaf(value)         => f(value)
        }
      }

      // Strictly speaking, this is not a tail-recursive function, thus it cannot be marked with tailrec annotation.
      // The proper tailrec function is really hard to write (see the commented section below).
      override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
        flatMap(f(a)) {
          case Left(value)  => tailRecM(value)(f)
          case Right(value) => pure(value)
        }
      }

      // A tailrec implementation.
//      def tailRecM[A, B](arg: A)
//                        (func: A => Tree[Either[A, B]]): Tree[B] = {
//        @tailrec
//        def loop(
//                  open: List[Tree[Either[A, B]]],
//                  closed: List[Option[Tree[B]]]): List[Tree[B]] =
//          open match {
//            case Branch(l, r) :: next =>
//              loop(l :: r :: next, None :: closed)
//            case Leaf(Left(value)) :: next =>
//              loop(func(value) :: next, closed)
//            case Leaf(Right(value)) :: next =>
//              loop(next, Some(pure(value)) :: closed)
//            case Nil =>
//              closed.foldLeft(Nil: List[Tree[B]]) { (acc, maybeTree) =>
//                maybeTree.map(_ :: acc).getOrElse {
//                  val left :: right :: tail = acc
//                  branch(left, right) :: tail
//                }
//              }
//          }
//        loop(List(func(arg)), Nil).head
//      }
    }

    import cats.syntax.functor._
    import cats.syntax.flatMap._

    val res1 = root.flatMap(node => leaf(node.toDouble))
    println(res1)

    val res2 = for {
      a <- root
      b <- branch(leaf(a - 1), leaf(a + 1))
    } yield b
    println(res2)


  }
}
