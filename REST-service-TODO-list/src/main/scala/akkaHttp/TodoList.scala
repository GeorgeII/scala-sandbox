package akkaHttp

import scala.collection.immutable.HashMap

// domain model
final case class Task(name: String, priority: Int)

object TodoList {

  // Imitates a storage, e.g. a database.
  var tasks: HashMap[String, Task] = HashMap.empty

  def addTask(task: Task): Unit = {
    tasks += (task.name -> task)
  }

  def getTasks: List[Task] = {
    tasks.values.toList.sortBy(_.priority)
  }

  def removeTask(taskName: String): Unit = {
    if (tasks.contains(taskName))
      tasks -= taskName
  }
}
