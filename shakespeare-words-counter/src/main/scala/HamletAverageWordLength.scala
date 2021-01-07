import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.io.Source

object HamletAverageWordLength{

  def main(args: Array[String]): Unit = {
    val numberOfFutures = 6

    // calculate every method N times and show the result of the last one. So, JVM becomes warmed up.
    val numberOfTests = 50

    val source = Source.fromURL("https://raw.githubusercontent.com/benschw/shakespeare-txt/master/shakespeare-hamlet-25.txt")

    // remove all punctuation marks except for ' and make list of words.
    val text = source.mkString
      .replaceAll("""[\p{Punct}&&[^']]|[\s]""", " ")
      .split(" ")
      .toList
    
    println(s"Number of words (including empty lines): ${text.length} \n")

    // in order to create some threads in threadpool before our main task
    val warmingUpExecutionContext = for{
      i <- 0 to numberOfFutures
    } yield Future {
      // Let our futures do some work. + i prevents from putting in the string pool
      "warm-up" * (100 + i)
    }
    warmingUpExecutionContext.map(Await.result(_, 3.second))


    // solving the task.

    var futuresRes: (Double, Long) = (0, 0)
    for {
      _ <- 0 to numberOfTests
    } futuresRes = time {
      // this block contains the inner logic of splitting/calculating/counting with Futures.

      // to create numberOfFutures Futures we need to calculate sliding-function parameters first
      val sublistSize = (text.length.toDouble / numberOfFutures).ceil.toInt

      val splitInLists = text.sliding(sublistSize, sublistSize).toVector

      val futures = for {
        sublist <- splitInLists
      } yield Future(findAverage(sublist))

      val (accumulatedLength, numberOfWords) = futures.map(Await.result(_, 3.second))
        .foldLeft((0.0, 0)) {
          case ((accTotalLength, accWordsQuantity), (x, y)) => (accTotalLength + x, accWordsQuantity + y)
        }

      val futuresResult = accumulatedLength / numberOfWords

      futuresResult
    }

    var straightforwardRes: (Double, Long) = (0, 0)
    for {
      _ <- 0 to numberOfTests
    } straightforwardRes = time {
      val (totalLength, totalWords) = findAverage(text)
      totalLength / totalWords
    }

    println(s"         Method          |      Result       |       Time")
    println(s"Futures                  | ${futuresRes._1} | ${futuresRes._2 / 1000000000.0} s")
    println(s"Straightforward approach | ${straightforwardRes._1} | ${straightforwardRes._2 / 1000000000.0} s")
  }


  /**
   * Returns accumulated words length and number of words.
   */
  def findAverage(words: Seq[String]): (Double, Int) = {
    // remove empty strings; make "John's" -> "Johns"; trim whitespaces on both sides " Hamlet   "  -> "Hamlet".
    val filtered = words
      .map(_.replaceAll("\'", ""))
      .filterNot(_.isBlank)
      .map(_.trim)

    (filtered.map(_.length).sum.toDouble, filtered.length)
  }

  /**
   * Returns the result of a code block passed into this function and its execution time.
   */
  def time[R](block: => R): (R, Long) = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    val executionTime = t1 - t0
    (result, executionTime)
  }
}
