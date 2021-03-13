package scalaWithCatsBook.chapter3


sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

// They wrote it in the solution. This allows us to create objects in the following way: Tree.leaf(100)
object Tree {

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = {
    Branch(left, right)
  }

  def leaf[A](value: A): Tree[A] = {
    Leaf(value)
  }
}

object Main {

  def main(args: Array[String]): Unit = {

    import cats.Functor
    import cats.syntax.functor._

    implicit val functorTree: Functor[Tree] = new Functor[Tree] {
      override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = {
        fa match {

          // recursively traverses its children
          case Branch(left, right) => Branch(map(left)(f), map(right)(f))

          case Leaf(value) => Leaf(f(value))
        }
      }
    }

    val tree1: Tree[Int] = Branch(Leaf(5), Leaf(6))
    val tree2: Tree[String] = Branch(Leaf("string"), Leaf("another string"))

    val tree1mapped = tree1.map(_.toDouble)
    val tree2mapped = tree2.map(_.toUpperCase)
    println(tree1mapped)
    println(tree2mapped)


    // This is what they added in the solution
    val mappedTree = Tree.leaf(100).map(_ * 2)
    println(mappedTree)


  }
}
