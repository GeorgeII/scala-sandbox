import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.Source
import scala.util.{Failure, Success}

object WordLengthCounter extends App {
  val numberOfFutures = 10

  val source = Source.fromURL("https://raw.githubusercontent.com/benschw/shakespeare-txt/master/shakespeare-hamlet-25.txt")
  val text = source.mkString
    //.replaceAll("""[,\s]+(|.*[^,\s])[,\s]+""", " ")
    .replaceAll("""[\p{Punct}&&[^']]|[\s]""", " ")
    .split(" ")
    //.filterNot(_.isBlank)
    //.map(_.trim)
    .toList

  text.foreach(println)
  println(text.length)

  // to create numberOfFutures Futures we need to calculate sliding-function parameters
  val sublistSize = (text.length.toDouble / numberOfFutures).ceil.toInt

  println(sublistSize)

  val splitInLists = text.sliding(sublistSize, sublistSize).toVector

  splitInLists.foreach(x => println(x.length))
  println(splitInLists.length)

  def processWords(words: Seq[String]): Future[Double] = Future {
    val filtered = words
      .map(_.replaceAll("\'", ""))
      .filterNot(_.isBlank)
      .map(_.trim)

    filtered.map(_.length).sum.toDouble / filtered.length
  }

  val futures = for{
    sublist <- splitInLists
  } yield processWords(sublist)

  //val processedFutures = futures.map(_.map(_))
  val result = futures
    .foldLeft(0)((acc, f) => f.onComplete {
      case Success(value) => value + acc
      case _ =>.0
    })
}
