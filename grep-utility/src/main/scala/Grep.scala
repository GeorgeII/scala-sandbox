import akka.actor.ActorSystem
import akka.stream.scaladsl._
import akka.util.ByteString

object Grep {

  def main(args: Array[String]): Unit = {

    val wordToFind = args(0)

    implicit val system: ActorSystem = ActorSystem("QuickStart")

    val source = StreamConverters.fromInputStream(() => System.in)

    val jvmHeapMaxMemory = Runtime.getRuntime.maxMemory()

    // Splits by '\n'; converts every line from bytes to string; filter strings.
    val flow = Framing
      .delimiter(
        delimiter = ByteString(System.lineSeparator()),
        maximumFrameLength = (jvmHeapMaxMemory * 0.8).toInt,
        allowTruncation = true
      )
      .map(_.utf8String)
      .filter(_.contains(wordToFind))

    val sink = Sink.foreach(println)

    // Potentially, can handle an endless input flow in the console.
    // And this is the reason it does not have a termination point. Only Ctrl+Z to force-stop the Scala application.
    source
      .via(flow)
      .to(sink)
      .run()
  }
}
