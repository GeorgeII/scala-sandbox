import akka.actor.ActorSystem
import akka.stream.scaladsl._
import akka.util.ByteString

import java.nio.file.Paths
import scala.io.StdIn.readLine

object Grep {

  def printStringsThatContainWord(strings: Vector[String], wordToFind: String): Unit = {
    for {
      string <- strings if string.contains(wordToFind)
    } println(string)
  }

  /**
   * I decided not to split it into 2 functions because that would double the number of if statements.
   * @return Empty String if the last character was '\n' or passed string otherwise.
   */
  def accumulateAndPrintLastString(lastCharInChunk: Byte, string: String, wordToFind: String): String = {
    if (lastCharInChunk == '\n') {
      printStringsThatContainWord(Vector(string), wordToFind)

      // means we don't need to accumulate and returns an empty string.
      ""
    }
    else {
      string
    }
  }

  def main(args: Array[String]): Unit = {

    println("Enter a word to find:")
    val wordToFind = readLine()
    println("Enter filename:")
    val filename = readLine()

    implicit val system: ActorSystem = ActorSystem("QuickStart")
    import system.dispatcher

    // a default chunk-size is 8192
    val source = FileIO.fromPath(Paths.get(filename))

    // this is a simple example of reading bytes by fixed-sized frames and waiting for a line separator.
    // it does not fit well 'cause it has a maximumFrameLength.
//    val flow = Framing
//      .delimiter(ByteString(System.lineSeparator()), maximumFrameLength = 512, allowTruncation = true)
//      .map(_.utf8String)


    val sink = Sink.fold[String, ByteString]("") {

      case (previousString, inputChunk) =>

        val strings = inputChunk
          .utf8String
          .split("\n")
          .toVector

        // handling a corner case: for instance, we are looking for a 'computer'. There is a possibility of reading a
        // file in the following chunk-order: '... this is my com' and 'puter\nAnd this is ...'. To print the entire line
        // we have to get the beginning of a string from the previous chunk and concatenate it with its continuation
        // in the next chunk.
        val firstString = previousString + strings.head
        val lastString = strings.last

        val preparedStringsToFindWord = firstString +: strings.slice(1, strings.length - 1)

        printStringsThatContainWord(preparedStringsToFindWord, wordToFind)

        // another corner case but for the last string in a chunk. If the string does not end with the '\n', we have to
        // concatenate it with the first string during the next step of 'fold'.
        val stringToPassToNextIteration = accumulateAndPrintLastString(inputChunk.last, lastString, wordToFind)

        stringToPassToNextIteration
    }

    source
      .runWith(sink)
      .onComplete(_ => system.terminate())
  }
}
