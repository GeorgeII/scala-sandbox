package day6

import scala.io.Source

object Main extends App{
  val filename = "src/main/scala/day6/input"
  val bufferedSource = Source.fromFile(filename)
  val groups: List[String] = bufferedSource.mkString("").split("\n\n").toList

  bufferedSource.close()

  var counter = 0

  for{
    group <- groups
  } {
    val groupAnswers = group.split("\n")
    var setOfAnswersInGroup = Set[Char]()

    for{
      person <- groupAnswers
    } {
      person.foreach(x => {setOfAnswersInGroup += x})
    }

    counter += setOfAnswersInGroup.size
  }

  println(counter)


  // part 2 starts here
  counter = 0

  for{
    group <- groups
  } {
    val groupAnswers = group.split("\n")
    val numberOfPeopleInGroup = groupAnswers.length
    val mapOfAnswersInGroup = scala.collection.mutable.Map[Char, Int]()

    for{
      person <- groupAnswers
    } {
      person.foreach(x => {
        mapOfAnswersInGroup(x) = if (mapOfAnswersInGroup.contains(x)) mapOfAnswersInGroup(x) + 1 else 1
      })
    }

    val numberOfCommonAnswers = mapOfAnswersInGroup.count((elem) => elem._2 == numberOfPeopleInGroup)
    counter += numberOfCommonAnswers
  }

  println(counter)
}

