package day3

import day3.Main.strings

import scala.io.Source

object Main extends App{
  val filename = "src/main/scala/day3/input"
  val bufferedSource = Source.fromFile(filename)
  val strings: List[String] = bufferedSource.getLines.toList

  bufferedSource.close()

  val lengthOfString = strings.head.length
  var counter = 0
  var positionInString = 0

  for{
    string <- strings
  } {
    if (string(positionInString) == '#') {
      counter += 1
    }

    if (positionInString + 3 < lengthOfString)
      positionInString += 3
    else positionInString = 2 - (lengthOfString - 1 - positionInString)
  }

  println(counter)


  // the second part of the assignment starts here.
  val first = countTreesWithGivenStep(1, strings)
  println(first)
  val second = countTreesWithGivenStep(3, strings)
  println(second)
  val third = countTreesWithGivenStep(5, strings)
  println(third)
  val fourth = countTreesWithGivenStep(7, strings)
  println(fourth)

  val stringsCutInHalf = strings.indices.collect { case i if i % 2 == 0 => strings(i) }.toList

  val fifth = countTreesWithGivenStep(1, stringsCutInHalf)
  println(fifth)

  println("Multiplication of all the results:")
  println(first * second * third * fourth * fifth)

  def countTreesWithGivenStep(stepSize: Int, strings: List[String]): Long = {
    val lengthOfString = strings.head.length
    var counter = 0
    var positionInString = 0

    for {
      string <- strings
    } {
      if (string(positionInString) == '#') {
        counter += 1
      }

      if (positionInString + stepSize < lengthOfString)
        positionInString += stepSize
      else positionInString = (stepSize - 1) - (lengthOfString - 1 - positionInString)
    }

    counter
  }
}

