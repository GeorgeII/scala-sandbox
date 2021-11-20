import cats.data.{ EitherNel, ValidatedNel }
import eu.timepit.refined.api.{ Refined, RefinedTypeOps }
import eu.timepit.refined.collection.Contains
import eu.timepit.refined.numeric.Greater
import eu.timepit.refined.predicates.all.NonEmpty
import eu.timepit.refined.refineV
import eu.timepit.refined.types.string.NonEmptyString
import io.estatico.newtype.macros.newtype

import scala.concurrent.Future

object StronglyTypedFunctions {

  case class User(firstName: String, lastName: String)

//  @newtype case class Username(value: String)
//  @newtype case class Email(value: String)

  type Username = String Refined Contains['g']

  type UserNameR = NonEmptyString
  object UserNameR extends RefinedTypeOps[UserNameR, String]

  type NameR = NonEmptyString
  object NameR extends RefinedTypeOps[NameR, String]

  type EmailR = String Refined Contains['@']
  object EmailR extends RefinedTypeOps[EmailR, String]

  @newtype case class UserName(value: UserNameR)
  @newtype case class Name(value: NameR)
  @newtype case class Email(value: EmailR)

  def main(args: Array[String]): Unit = {

    // for @newtype's
    //    def lookup(username: Username, email: Email): Future[Option[User]] = Future(Option(User(username.toString, email.toString)))

    def lookup(username: Username): Future[Option[User]] = Future(Option(User(username.toString, "randomLastName")))

    // even though IDEA's linter thinks it's wrong - it is compilable and correct code
    lookup("aeginstein")

    // runtime features
    println("Runtime features:")
    val str: String = "some runtime value"

    val res: Either[String, NonEmptyString] =
      refineV[NonEmpty](str)

    val emptyStr = ""
    val emptyRes = refineV[NonEmpty](emptyStr)
    // equivalently:
    // val r = NonEmptyString.from(emptyStr)

    println(res)
    println(emptyRes)
    res.foreach(println)
    emptyRes.foreach(println)

    type GTFive = Int Refined Greater[5]
    object GTFive extends RefinedTypeOps[GTFive, Int]

    val number                                    = 33
    val two                                       = 2
    val greaterRes: Either[String, GTFive]        = GTFive.from(number)
    val greaterInvalidRes: Either[String, GTFive] = GTFive.from(two)

    println(greaterRes)
    println(greaterInvalidRes)

    case class MyType(a: NonEmptyString, b: GTFive)
    def validate(a: String, b: Int): ValidatedNel[String, MyType] =
      (
        NonEmptyString.from(a).toValidatedNel,
        GTFive.from(b).toValidatedNel
      ).mapN(MyType.apply)

    case class Person(
        username: UserName,
        name: Name,
        email: Email
    )

    def mkPerson(
        u: String,
        n: String,
        e: String
    ): EitherNel[String, Person] =
      (
        UserNameR.from(u).toEitherNel.map(UserName.apply),
        NameR.from(n).toEitherNel.map(Name.apply),
        EmailR.from(e).toEitherNel.map(Email.apply)
      ).parMapN(Person.apply)
  }

}
