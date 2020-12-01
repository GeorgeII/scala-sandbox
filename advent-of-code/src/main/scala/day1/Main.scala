package day1

import scala.io.Source

object Main extends App {
  val filename = "src/main/scala/day1/input"
  val bufferedSource = Source.fromFile(filename)

  val values: Set[Int] = bufferedSource.getLines.map(_.toInt).toSet

  bufferedSource.close()

  var neededValue = -1

  for{
    item <- values
  } {
    if (values.contains(2020 - item)) {
      neededValue = item
    }
  }

  println(neededValue * (2020 - neededValue))


  // second part of the assignment starts here.
  var secondNeededValue = -1

  for{
    item <- values
  } {
    val newSet = values - item
    val newSum = 2020 - item

    for{
      item2 <- newSet
    } {
      if (newSet.contains(newSum - item2)) {
        neededValue = item
        secondNeededValue = newSum - item2
      }
    }
  }
  println(neededValue * secondNeededValue * (2020 - neededValue - secondNeededValue))

}
