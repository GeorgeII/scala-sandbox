package akkaHttp

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

// unmarshalling and marshalling
trait TaskJsonProtocol extends DefaultJsonProtocol {
  implicit val taskFormat: RootJsonFormat[Task] = jsonFormat2(Task)
}

object Server extends TaskJsonProtocol with SprayJsonSupport {

  implicit val actorSystem = ActorSystem(Behaviors.empty, "AkkaHttpSystem")

  val routes: Route = {
    concat(
      (path("") & get) {
        complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "<h1>The server is up! Now add tasks to /api/tasks via " +
          "POST and check them out via GET</h1>"))
      },

      (path("api" / "tasks") & get) {
        complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "<h1>TODO: add TODO list ;)</h1>"))
      },

      (path("api" / "tasks") & post) {
        entity(as[Task]) { task =>
          complete(Task("5", 10))
        }
      }
    )
  }

  def main(args: Array[String]): Unit = {
    Http().newServerAt("localhost", 8080).bind(routes)
  }
}
