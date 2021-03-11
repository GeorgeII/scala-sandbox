// An implementation of a basic type class using multiple approaches to declare implicits.

package scalaWithCatsBook.chapter1

trait Printable[A] {
  def format(value: A): String
}

object Printable {

  def format[A](value: A)(implicit printable: Printable[A]): String = {
    printable.format(value)
  }

  def print[A](value: A)(implicit printable: Printable[A]): Unit = {
    println(printable.format(value))
  }

}

// domain
final case class Cat(name: String, age: Int, color: String)


object Main {

  def main(args: Array[String]): Unit = {

    import PrintableInstances._

    val fromInt: String = Printable.format(5)
    val fromStr: String = Printable.format("string")

    Printable.print(10)
    Printable.print("another string")


    // Now check for the custom type Cat.

    // We can easily 'extend' our already existing 'library' just by declaring an implicit of the convenient type.
    // The declaration happens on the client-side of the app (we do not alter Printable type-class) which makes it
    // very flexible for extensions.
    implicit val catPrintable = new Printable[Cat] {
      override def format(value: Cat): String = {
        s"${value.name} is a ${value.age} year-old ${value.color} cat."
      }
    }

    val cat = Cat("Tom", 7, "black")
    val catStr: String = Printable.format(cat)
    Printable.print(cat)


    // Now using PrintableOps.
    import PrintableSyntax._

    // The old name for this kind of adding methods to the type is 'pimping' or 'type enrichment' (there's a common
    // functional-programming pattern called Pimp My Library). But nowadays these names are considered old.
    val strOpsCat: String = cat.format
    cat.print
  }
}
