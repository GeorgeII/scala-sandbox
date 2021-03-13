package scalaWithCatsBook.chapter2

import cats.Monoid
import cats.syntax.semigroup._


case class Order(totalCost: Double, quantity: Double)

object Main {

  def add[A](items: List[A])(implicit monoid: Monoid[A]) : A = {
    println(monoid)
    items.reduce(_ |+| _)
  }

  def main(args: Array[String]): Unit = {

    import cats.instances.int._
    import cats.instances.option._

    println(add(List(1, 2, 3)))
    println(add(List(Option(1), Option(2), Option(3), None)))


    import cats.instances.double._

    implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
      override def empty: Order = Order(0, 0)

      override def combine(x: Order, y: Order): Order = Order(x.totalCost |+| y.totalCost, x.quantity |+| y.quantity)
    }

    val order1 = Order(5.2, 3.5)
    val order2 = Order(2.1, 7)

    println(add(List(order1, order2)))
  }
}
