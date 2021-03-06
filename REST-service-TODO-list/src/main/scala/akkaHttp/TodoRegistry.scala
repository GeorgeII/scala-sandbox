package akkaHttp

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}

object TodoRegistry {

  sealed trait Command

  final case class GetTodoTasks(replyTo: ActorRef[List[Task]]) extends Command
  final case class CreateTodoTask(task: Task, replyTo: ActorRef[ActionPerformed]) extends Command
  final case class RemoveTodoTask(taskName: String, replyTo: ActorRef[ActionPerformed]) extends Command

  // response
  final case class ActionPerformed(description: String)

  def apply(): Behavior[Command] = {
    Behaviors.receiveMessage {
      case GetTodoTasks(replyTo)   =>
        replyTo ! TodoList.getTasks
        Behaviors.same

      case CreateTodoTask(task, replyTo) =>
        TodoList.addTask(task)
        replyTo ! ActionPerformed(s"Task ${task.name} was successfully added.")
        Behaviors.same

      case RemoveTodoTask(taskName, replyTo) =>
        TodoList.removeTask(taskName)
        replyTo ! ActionPerformed(s"Task $taskName was successfully removed from the list")
        Behaviors.same
    }
  }

}
