package redBook.chapter3

import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeExercises {

  def size[A](node: Tree[A]): Int = {
    node match {
      case Leaf(_)             => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }
  }

  def maximum(node: Tree[Int]): Int = {
    node match {
      case Leaf(value)         => value
      case Branch(left, right) => maximum(left) max maximum(right)
    }
  }

  def depth[A](node: Tree[A]): Int = {
    def helper(acc: Int, innerNode: Tree[A]): Int =
      innerNode match {
        case Leaf(_)             => acc
        case Branch(left, right) => helper(acc + 1, left) max helper(acc + 1, right)
      }

    helper(0, node)
  }

  def map[A, B](node: Tree[A])(f: A => B): Tree[B] = {
    node match {
      case Leaf(value)         => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }


  def main(args: Array[String]): Unit = {

    println("Test 'size' function")
    println(size(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4))))

    println("Test 'maximum' function")
    println(maximum(Branch(Branch(Leaf(1), Branch(Leaf(8), Leaf(3))), Leaf(4))))

    println("Test 'depth' function")
    println(depth(Branch(Branch(Leaf(1), Branch(Leaf(8), Leaf(3))), Leaf(4))))

    println("Test 'map' function")
    println(map(Branch(Branch(Leaf(1), Branch(Leaf(8), Leaf(3))), Leaf(4)))(_.toDouble))
  }
}
