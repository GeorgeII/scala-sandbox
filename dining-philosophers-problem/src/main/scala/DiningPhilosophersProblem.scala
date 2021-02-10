import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}

import scala.util.Random

object DiningPhilosophersProblem {

  final case class Forks()

  object Philosopher {

    final case class StartEating(forks: Forks, myName: String, replyTo: ActorRef[Orchestrator.Command])

    def apply(): Behavior[StartEating] = {
      Behaviors.receiveMessage {
        case StartEating(forks, myName, replyTo) =>

          // a philosopher eats for 2 seconds
          println("I got both forks! I am eating...")
          println(s"By the way, I am ${myName}")
          Thread.sleep(3 * 1000)
          println("I'm full. Take my forks back! \n")
          Thread.sleep(2 * 1000)

          // then he gives the forks back to the waiter
          replyTo ! Orchestrator.GiveForks(forks)
          Behaviors.same
      }
    }
  }


  object Orchestrator {

    sealed trait Command
    final case class GiveForks(forks: Forks) extends Command

    def apply(): Behavior[Command] = {
      Behaviors.setup[Command] { context =>
        val philosopher1: ActorRef[Philosopher.StartEating] = context.spawn(Philosopher(), "philosopher-1")
        val philosopher2: ActorRef[Philosopher.StartEating] = context.spawn(Philosopher(), "philosopher-2")
        val philosopher3: ActorRef[Philosopher.StartEating] = context.spawn(Philosopher(), "philosopher-3")
        val philosopher4: ActorRef[Philosopher.StartEating] = context.spawn(Philosopher(), "philosopher-4")
        val philosopher5: ActorRef[Philosopher.StartEating] = context.spawn(Philosopher(), "philosopher-5")

        Behaviors.receiveMessage {
          case GiveForks(forks) =>
            println("I am an Orchestrator and I just got 2 forks! Hmm, let me choose a philosopher to eat...")

            // a philosopher to eat is being chosen randomly. As a default RNG has a uniform distribution it guarantees
            // each philosopher to be fed in the long term.
            val philosophers = List(philosopher1, philosopher2, philosopher3, philosopher4, philosopher5)
            val random = new Random
            val replyTo = philosophers(
              random.nextInt(philosophers.length)
            )
            println("I think I'll give them to this one. \n")
            Thread.sleep(2 * 1000)

            replyTo ! Philosopher.StartEating(forks, replyTo.path.name, context.self)
            Behaviors.same
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    // the 2 forks which we are passing during the program execution
    val forks = Forks()

    val orchestrator = ActorSystem(Orchestrator(), "orchestrator")

    orchestrator ! Orchestrator.GiveForks(forks)

    Thread.sleep(100 * 1000)
    orchestrator.terminate()

    println("\n Program is finished")
  }
}
