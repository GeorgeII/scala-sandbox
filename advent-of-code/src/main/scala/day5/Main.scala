package day5

import scala.io.Source

object Main extends App{
  val filename = "src/main/scala/day5/input"
  val bufferedSource = Source.fromFile(filename)
  val strings: List[String] = bufferedSource.getLines.toList

  bufferedSource.close()

  var max = -1

  def cutInHalf(tightLowerBound: Int, tightUpperBound: Int, signOfUpperHalf: Char, pass: String): Int = {
    var lower = tightLowerBound
    var upper = tightUpperBound
    var value = -1

    for{
      char <- pass
    } {
      if (char == signOfUpperHalf) {
        lower = ((lower + upper).toDouble / 2).ceil.toInt
        value = lower
      } else {
        upper = ((lower + upper).toDouble / 2).floor.toInt
        value = upper
      }
    }

    value
  }

  for{
    pass <- strings
  } {
    val row = pass.substring(0, 7)
    val column = pass.substring(7)

    val foundRow = cutInHalf(0, 127, 'B', row)
    val foundColumn = cutInHalf(0, 7, 'R', column)

    val seatId = foundRow * 8 + foundColumn

    if (seatId > max)
      max = seatId
  }

  println(max)


  // the 2nd part of the assignment starts here
  val seats = Array.fill[Int](128, 8) {-1}

  for{
    pass <- strings
  } {
    val row = pass.substring(0, 7)
    val column = pass.substring(7)

    val foundRow = cutInHalf(0, 127, 'B', row)
    val foundColumn = cutInHalf(0, 7, 'R', column)

    val seatId = foundRow * 8 + foundColumn
    seats(foundRow)(foundColumn) = seatId
  }

  val reducedSeats = seats.flatMap(_.toList).dropWhile(_ == -1).reverse.dropWhile(_ == -1).reverse
  val mySeatIndex = reducedSeats.indexOf(-1)
  val mySeat = reducedSeats(mySeatIndex + 1) - 1

  println(mySeat)
}

