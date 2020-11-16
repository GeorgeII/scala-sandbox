package com.github.georgeii

object Application extends App {
  println("Enter the week number you want to scrape or press enter to scrape the latest available week:")
  /*val weekNumber: String = readLine()

  val url: String = "https://blog.reedsy.com/creative-writing-prompts/contests/" +
    {if (!weekNumber.isBlank) weekNumber else getLatestAvailableWeekNumber}
  */

  val (weekInfo: WeekInfo, stories: List[StoryModel]) = Utils.getWeekInfoAndStories("url")

  Utils.writeWeekInfoToDatabase(weekInfo)
  Utils.writeStoriesToDatabase(stories)
}
