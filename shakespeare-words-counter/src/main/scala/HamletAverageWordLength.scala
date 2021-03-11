import java.util.concurrent.ForkJoinPool
import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.Source

object HamletAverageWordLength{

  def main(args: Array[String]): Unit = {
    val numberOfFutures = Runtime.getRuntime.availableProcessors()
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(new ForkJoinPool(numberOfFutures))

    val source = Source.fromURL("https://raw.githubusercontent.com/benschw/shakespeare-txt/master/shakespeare-hamlet-25.txt")

    // remove all punctuation marks except for ' and make a vector of words.
    val text = source.mkString
      .replaceAll("""[\p{Punct}&&[^']]|[\s]""", " ")
      .split(" ")
      .toVector

    source.close()
    
    println(s"Number of words (including empty lines): ${text.length} \n")

    // to create numberOfFutures Futures we need to calculate sliding-function parameters first
    val sublistSize = text.length / numberOfFutures + 1
    val splitInLists = text.sliding(sublistSize, sublistSize).toVector

//    val cleanEachWord = (words: Vector[String]) => {
//      words
//        .map(_.replaceAll("\'", ""))
//        .filterNot(_.isBlank)
//        .map(_.trim)
//    }
//    val sumLengthLength = (words: Vector[String]) => (words.map(_.length).sum, words.length)
//    val cleanEachWordAndThenSumLengthLength = cleanEachWord andThen sumLengthLength

    val futures = for {
      sublist <- splitInLists
    } yield Future(cleanEachWordAndThenSumLengthLength(sublist))

    val futureSequence = Future.sequence(futures)

    // the only Await in the entire program.
    val unwrappedSequence = Await.result(futureSequence, 3.seconds)

    val (accumulatedLength, numberOfWords) = unwrappedSequence.foldLeft((0.0, 0L)) {
        case ((accTotalLength, accWordsQuantity), (x, y)) => (accTotalLength + x, accWordsQuantity + y)
      }
    val futuresResult = accumulatedLength / numberOfWords


    val (totalLength, totalWords) = cleanEachWordAndThenSumLengthLength(text)
    val straightforwardRes = totalLength.toDouble / totalWords

    println(s"Futures result: $futuresResult")
    println(s"Straightforward result: $straightforwardRes")
  }


  /**
   * Returns accumulated words length and number of words.
   */
  def cleanEachWordAndThenSumLengthLength: Vector[String] => (Long, Long) = {
    cleanEachWord andThen sumLengthLength
  }

  /**
   *  removes empty strings; make "John's" -> "Johns"; trim whitespaces on both sides " Hamlet   "  -> "Hamlet".
   */
  def cleanEachWord: Vector[String] => Vector[String] = { words =>
    words
      .map(_.replaceAll("\'", ""))
      .filterNot(_.isBlank)
      .map(_.trim)
  }

  def sumLengthLength: Vector[String] => (Long, Long) = { words =>
    (words.map(_.length).sum, words.length)
  }
}
