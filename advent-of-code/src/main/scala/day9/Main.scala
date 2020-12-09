package day9

import scala.io.Source

object Main extends App{
  val filename = "src/main/scala/day9/input"
  val bufferedSource = Source.fromFile(filename)
  val input: List[Long] = bufferedSource.getLines().map(_.toLong).toList

  bufferedSource.close()

  val numbers = input.slice(25, input.length)
  var preamble = input.slice(0, 25)
  var numberFound = -1L

  for{
    number <- numbers
  } {
    var isSum = false

    for{
      numberInPreamble <- preamble
    } {
      if (preamble.contains(number - numberInPreamble))
        isSum = true
    }

    if (!isSum) {
      println(number)
      numberFound = number
    }

    preamble = preamble.tail :+ number
  }


  // part 2 starts here
  val candidateNumbers = input.slice(0, input.indexOf(numberFound)) ::: input.slice(input.indexOf(numberFound) + 1, input.length)
  var listFound: List[Long] = List.empty

  for {
    idx <- 0 until candidateNumbers.length - 1
  } {
    var listToSum = List(candidateNumbers(idx), candidateNumbers(idx + 1))
    var sum = listToSum.sum

    var innerCounter = idx + 2
    while (sum < numberFound || innerCounter < candidateNumbers.length) {
      if (sum == numberFound) {
        listFound = listToSum
      }

      listToSum = listToSum :+ candidateNumbers(innerCounter)
      sum += candidateNumbers(innerCounter)
      innerCounter += 1
    }
  }

  println(listFound.max + listFound.min)
}

