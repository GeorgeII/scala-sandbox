import akka.actor.ActorSystem
import akka.stream.scaladsl._
import akka.util.ByteString

import java.nio.file.Paths

object Grep {

  def main(args: Array[String]): Unit = {
    if (args.length != 2)
      sys.error("You must pass 2 arguments: a word to find and a file")

    implicit val system: ActorSystem = ActorSystem("QuickStart")
    import system.dispatcher

    val wordToFind = args(0)
    val filename = args(1)

    val source = FileIO.fromPath(Paths.get(filename), chunkSize = 1)

    // this is a simple example of reading bytes by fixed-sized frames and waiting for a line separator.
//    val flow = Framing
//      .delimiter(ByteString(System.lineSeparator()), maximumFrameLength = 512, allowTruncation = true)
//      .map(_.utf8String)


    val sink = Sink.fold[ByteString, ByteString](ByteString("")) {
      case (line, inputChunk) if inputChunk == ByteString("\n") =>
        if (line.utf8String.contains(wordToFind))
          println(line.utf8String)
        // start accumulating from an empty string again
        ByteString("")

      case (line, inputChunk) =>
        line ++ inputChunk
    }

    source
      .runWith(sink)
      .onComplete(_ => system.terminate())
  }
}
