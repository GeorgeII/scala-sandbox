package scalaWithCatsBook.chapter1

object PrintableInstances {

  implicit val stringPrintable = new Printable[String] {
    override def format(value: String): String = {
      value
    }
  }

  implicit val intPrintable = new Printable[Int] {
    override def format(value: Int): String = {
      value.toString
    }
  }

}
