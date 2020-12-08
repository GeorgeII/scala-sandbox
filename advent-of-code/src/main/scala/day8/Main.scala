package day8

import scala.annotation.tailrec
import scala.io.Source

object Main extends App{
  val filename = "src/main/scala/day8/input"
  val bufferedSource = Source.fromFile(filename)
  val commands: List[String] = bufferedSource.getLines().toList

  bufferedSource.close()

  val numberOfCommands = commands.length
  var accumulator = 0
  var setOfAlreadyExecutedLines = scala.collection.mutable.Set[Int]()

  traverseCommands(0, commands)

  @tailrec
  def traverseCommands(numberOfLine: Int, commands: List[String]): Unit = {
    val currentLine = commands(numberOfLine)

    if (currentLine.isBlank) {
      executedCorrectly = true
      return
    }

    if (setOfAlreadyExecutedLines.contains(numberOfLine))
      return
    else setOfAlreadyExecutedLines += numberOfLine

    val command = currentLine.substring(0, 3)
    val value = currentLine.substring(currentLine.indexOf(" ") + 1)

    command match {
      case "nop" => traverseCommands(numberOfLine + 1, commands)

      case "acc" =>
        accumulator += value.toInt
        traverseCommands(numberOfLine + 1, commands)

      case "jmp" => traverseCommands(numberOfLine + value.toInt, commands)
    }
  }

  println(accumulator)



  // part 2 starts here
  accumulator = 0
  val indicesOfNop = commands.zipWithIndex.filter(nop => nop._1.substring(0, 3) == "nop").map(_._2)
  val indicesOfJmp = commands.zipWithIndex.filter(jmp => jmp._1.substring(0, 3) == "jmp").map(_._2)
  var executedCorrectly = false

  for{
    indexOfNop <- indicesOfNop
  } {
    accumulator = 0
    setOfAlreadyExecutedLines = scala.collection.mutable.Set[Int]()
    val currentLine = commands(indexOfNop)
    val newListWithChangedCommand = commands.slice(0, indexOfNop) :::
      List("jmp" + currentLine.substring(3)) ::: commands.slice(indexOfNop + 1, numberOfCommands) ::: List("")

    traverseCommands(0, newListWithChangedCommand)

    if (executedCorrectly) {
      println(accumulator)
      executedCorrectly = false
    }
  }

  for{
    indexOfNop <- indicesOfJmp
  } {
    accumulator = 0
    setOfAlreadyExecutedLines = scala.collection.mutable.Set[Int]()
    val currentLine = commands(indexOfNop)
    val newListWithChangedCommand = commands.slice(0, indexOfNop) :::
      List("nop" + currentLine.substring(3)) ::: commands.slice(indexOfNop + 1, numberOfCommands) ::: List("")

    traverseCommands(0, newListWithChangedCommand)

    if (executedCorrectly) {
      println(accumulator)
      executedCorrectly = false
    }
  }
}

