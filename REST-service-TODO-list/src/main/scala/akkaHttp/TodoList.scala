package akkaHttp

import spray.json.DefaultJsonProtocol._
import spray.json.RootJsonFormat

import scala.collection.immutable.HashMap

// domain model
final case class Task(name: String, priority: Int)

object TodoList {

  var tasks: HashMap[String, Task] = HashMap.empty

  // unmarshalling and marshalling
  implicit val taskFormat: RootJsonFormat[Task] = jsonFormat2(Task)

  def addTask(task: Task): Unit = {
    tasks += (task.name -> task)
  }

  def getTasks: List[Task] = {
    tasks.values.toList.sortBy(_.priority)
  }
}
