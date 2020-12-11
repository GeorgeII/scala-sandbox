package day10

import scala.io.Source

object Main extends App{
  val filename = "src/main/scala/day10/input"
  val bufferedSource = Source.fromFile(filename)
  val adapters: Vector[Int] = bufferedSource.getLines().map(_.toInt).toVector.sortWith(_ > _)

  bufferedSource.close()

  val highestRatedAdapter = adapters(0) + 3

  val differenceBetweenNeighbors = (highestRatedAdapter +: adapters :+ 0).sliding(2).toList
                                    .map(neighbors => neighbors(0) - neighbors(1))

  val differByOne = differenceBetweenNeighbors.count(_ == 1)
  val differByThree = differenceBetweenNeighbors.count(_ == 3)

  println(differByOne * differByThree)

  // part 2.
  val t1 = System.nanoTime
  val cache = scala.collection.mutable.HashMap[Int, Long]()

  val numberOfAllCombinations = countCombinations(adapters.toList :+ 0)
  println(numberOfAllCombinations)

  val duration = (System.nanoTime - t1) / 1e9d
  println(s"Part2 execution time: $duration")

  def countCombinations(remainedAdapters: List[Int]): Long = {
    if (cache.contains(remainedAdapters.head))
      return cache(remainedAdapters.head)

    val valueToCache = remainedAdapters.length match {
      case x if x > 3 =>
        var jumpThree = 0L
        var jumpTwo = 0L
        var jumpOne = 0L
        if (remainedAdapters.head - remainedAdapters(3) <= 3)
          jumpThree = countCombinations(remainedAdapters.drop(3))
        if (remainedAdapters.head - remainedAdapters(2) <= 3)
          jumpTwo = countCombinations(remainedAdapters.drop(2))
        if (remainedAdapters.head - remainedAdapters(1) <= 3)
          jumpOne = countCombinations(remainedAdapters.drop(1))

        jumpOne + jumpTwo + jumpThree

      case 3 =>
        var jumpTwo = 0L
        var jumpOne = 0L
        if (remainedAdapters.head - remainedAdapters(2) <= 3)
          jumpTwo = countCombinations(remainedAdapters.drop(2))
        if (remainedAdapters.head - remainedAdapters(1) <= 3)
          jumpOne = countCombinations(remainedAdapters.drop(1))

        jumpOne + jumpTwo

      case 2 =>
        if (remainedAdapters.head - remainedAdapters(1) <= 3)
          countCombinations(remainedAdapters.drop(1))
        else 0L

      case 1 => 1
    }

    cache(remainedAdapters.head) = valueToCache

    valueToCache
  }
}

