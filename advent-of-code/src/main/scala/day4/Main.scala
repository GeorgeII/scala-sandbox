package day4

import scala.io.Source
import scala.util.control.Breaks._

object Main extends App{
  val filename = "src/main/scala/day4/input"
  val bufferedSource = Source.fromFile(filename)
  val strings = bufferedSource.mkString("").split("\n\n").toList

  bufferedSource.close()

  val requiredFields = scala.collection.mutable.Set("hcl", "ecl", "byr", "iyr", "eyr", "hgt", "pid")
  var counter = 0

  for{
    string <- strings
  } {
    val fields = string.replace("\n", " ").split(" ").map(_.substring(0, 3)).filter(_ != "cid")

    if (requiredFields.forall(field => fields.contains(field))) {
      counter += 1
    }
  }

  println(counter)



  // the second part of the assignment starts here.
  counter = 0

  for{
    string <- strings
  } {
    val fields = string.replace("\n", " ").split(" ")
      .map(field => field.substring(0, 3) -> field.substring(4)).filter(_._1 != "cid").toMap

    if (requiredFields.forall(field => fields.contains(field))) {

      val cond1 = fields("byr").toInt >= 1920 && fields("byr").toInt <= 2002
      val cond2 = fields("iyr").toInt >= 2010 && fields("iyr").toInt <= 2020
      val cond3 = fields("eyr").toInt >= 2020 && fields("eyr").toInt <= 2030

      var regex = "#[a-f0-9]{6}".r
      val cond4 = regex.findFirstIn(fields("hcl")).getOrElse("").nonEmpty

      val allowedEyeColor = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
      val cond5 = allowedEyeColor.contains(fields("ecl"))

      regex = "[0-9]{9}".r
      val cond6 = regex.findFirstIn(fields("pid")).getOrElse("").nonEmpty

      val indexOfUnits = if (fields("hgt").indexOf("cm") != -1) fields("hgt").indexOf("[a-z]") else fields("hgt").indexOf("in")
      val units = fields("hgt").substring(indexOfUnits)
      val height = fields("hgt").substring(0, indexOfUnits).toInt
      val cond7 = if (units == "cm") {height >= 150 && height <= 193} else {height >= 59 && height <= 76}


      if (cond1 && cond2 && cond3 && cond4 && cond5 && cond6 && cond7) {
        counter += 1
      }
    }
  }

  println(counter)
}

