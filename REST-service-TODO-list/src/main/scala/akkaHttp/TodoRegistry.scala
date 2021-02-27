package akkaHttp

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}

object TodoRegistry {

  sealed trait Command
  final case class GetTodoTasks(replyTo: ActorRef[List[Task]]) extends Command
  final case class CreateTodoTask(task: Task, replyTo: ActorRef[ActionPerformed]) extends Command

  // response
  final case class ActionPerformed(description: String)

  private def registry(tasks: Set[Task]): Behavior[Command] = {
    Behaviors.receiveMessage {
      case GetTodoTasks(replyTo)   =>
        replyTo ! tasks.toList
        Behaviors.same
      case CreateTodoTask(task, replyTo) =>
        replyTo ! ActionPerformed(s"Task ${task.name} was successfully added.")
        registry(tasks + task)
    }
  }

  def apply(): Behavior[Command] = registry(Set.empty)

}
