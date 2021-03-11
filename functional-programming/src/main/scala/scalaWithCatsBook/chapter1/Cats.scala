package scalaWithCatsBook.chapter1

object Cats {

  def main(args: Array[String]): Unit = {

    import cats.Show
    import cats.syntax.show._

    // built-in analog of Printable
    implicit val catShow: Show[Cat] = {
      Show.show(cat => s"${cat.name} is a ${cat.age} year-old ${cat.color} cat.")
    }

    val cat = Cat("Luis", 6, "brown")
    val strCat: String = cat.show
    println(strCat)



    import cats.Eq
    import cats.instances.int._
    import cats.instances.string._
    import cats.syntax.eq._

    // Eq for a type-safe comparison
    implicit val catEq: Eq[Cat] = Eq.instance(
      (cat1, cat2) =>
        (cat1.name === cat2.name) && (cat1.age === cat2.age) && (cat1.color === cat2.color)
    )

    val cat1 = Cat("Garfield", 38, "orange and black")
    val cat2 = Cat("Heathcliff", 33, "orange and black")

    println(cat1 === cat1)
    println(cat1 === cat2)


    import cats.instances.option._

    val optionCat1 = Option(cat1)
    val optionCat2 = Option.empty[Cat]

    println(optionCat1 === optionCat1)
    println(optionCat1 === optionCat2)

  }
}
