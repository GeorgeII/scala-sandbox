package standaloneExamples

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.Contains
import eu.timepit.refined.auto._

import io.estatico.newtype.macros._

object StronglyTypedFunctions {

  case class User(firstName: String, lastName: String)

//  @newtype case class Username(value: String)
  @newtype case class Email(value: String)


  type Username = String Refined Contains['g']

  def main(args: Array[String]): Unit = {

    // for @newtype's
    //    def lookup(username: Username, email: Email): Future[Option[User]] = Future(Option(User(username.toString, email.toString)))

    def lookup(username: Username): Future[Option[User]] = Future(Option(User(username.toString, "randomLastName")))


    // even though IDEA's linter thinks it's wrong - it is compilable and correct code
    lookup("aeginstein")

  }

}
