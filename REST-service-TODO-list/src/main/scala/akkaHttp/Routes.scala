package akkaHttp

import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.Directives._
import akka.util.Timeout
import akkaHttp.TodoRegistry.{ActionPerformed, CreateTodoTask, GetTodoTasks, RemoveTodoTask}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route

import scala.concurrent.Future

class Routes(tasksRegistry: ActorRef[TodoRegistry.Command])(implicit val system: ActorSystem[_])
  extends JsonEncoders
    with SprayJsonSupport {

  // timeout for a request
  private implicit val timeout: Timeout = Timeout.create(java.time.Duration.ofSeconds(5))

  def getTasks: Future[List[Task]] = {
    tasksRegistry.ask(GetTodoTasks)
  }

  def createTask(task: Task): Future[ActionPerformed] = {
    tasksRegistry.ask(CreateTodoTask(task, _))
  }

  def removeTask(taskName: String): Future[ActionPerformed] = {
    tasksRegistry.ask(RemoveTodoTask(taskName, _))
  }


  // paths

  val routes: Route = {
    concat(
      (path("") & get) {
        complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "<h1>The server is up! Now add tasks to /api/tasks via " +
          "POST and check them out via GET. You can also remove tasks from the list.</h1>"))
      },

      (path("api" / "tasks") & get) {
        complete(StatusCodes.OK, getTasks)
      },

      (path("api" / "tasks") & post) {
        entity(as[Task]) { task =>
          onSuccess(createTask(task)) { performed =>
            complete(StatusCodes.Created, performed)
          }
        }
      },

      (path("api" / "tasks") & delete) {
        entity(as[String]) { taskName =>
          onSuccess(removeTask(taskName)) { performed =>
            complete(StatusCodes.OK, performed)
          }
        }
      }
    )
  }

}
