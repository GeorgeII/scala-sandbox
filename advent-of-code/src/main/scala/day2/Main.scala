package day2

import scala.io.Source

object Main extends App{
  val filename = "src/main/scala/day2/input"
  val bufferedSource = Source.fromFile(filename)
  val values: List[String] = bufferedSource.getLines.toList

  var counter = 0

  for{
    entry <- values
  } {
    val indexOfDash = entry.indexOf("-")
    val firstWhitespate = entry.indexOf(" ")
    val colon = entry.indexOf(":")

    val minimum = entry.substring(0, indexOfDash).toInt
    val maximum = entry.substring(indexOfDash + 1, firstWhitespate).toInt
    val letter = entry.substring(firstWhitespate + 1, colon).charAt(0)
    val password = entry.substring(colon + 2)
    val occurrences = password.count(_ == letter)

    if (occurrences <= maximum && occurrences >= minimum) {
      counter += 1
    }
  }

  println(counter)



  // the second part of the assignment starts here.

  counter = 0

  for{
    entry <- values
  } {
    val indexOfDash = entry.indexOf("-")
    val firstWhitespate = entry.indexOf(" ")
    val colon = entry.indexOf(":")

    val firstPosition = entry.substring(0, indexOfDash).toInt - 1
    val secondPosition = entry.substring(indexOfDash + 1, firstWhitespate).toInt - 1
    val letter = entry.substring(firstWhitespate + 1, colon).charAt(0)
    val password = entry.substring(colon + 2)

    if ((password(firstPosition) == letter) ^ (password(secondPosition) == letter)) {
      counter += 1
    }
  }

  println(counter)

}
