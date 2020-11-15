package com.github.georgeii

import java.io.{File, PrintWriter}
import java.time.LocalDateTime

import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import org.jsoup.select.Elements

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine

object Application extends App {
  println("Enter the week number you want to scrape or press enter to scrape the latest available week:")
  /*val weekNumber: String = readLine()

  val url: String = "https://blog.reedsy.com/creative-writing-prompts/contests/" +
    {if (!weekNumber.isBlank) weekNumber else getLatestAvailableWeekNumber}
  */

  val (weekInfo: WeekInfo, stories: List[StoryModel]) = Utils.getWeekInfoAndStories("qwe")


  /*val doc: Document = Jsoup.connect().get()

  println(doc.title())*/
}
