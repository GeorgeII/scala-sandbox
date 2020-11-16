package com.github.georgeii

import java.io.File
import java.time.LocalDateTime

import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import org.mongodb.scala.bson.collection.immutable.{Document => MongoDocument}
import org.mongodb.scala.{Completed, MongoClient, MongoCollection, MongoDatabase, Observable, Observer}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Promise}
import scala.jdk.CollectionConverters._

object Utils {
  def getWeekInfoAndStories(url: String): (WeekInfo, List[StoryModel]) = {
    //val doc: Document = Jsoup.connect("https://blog.reedsy.com/creative-writing-prompts/contests/67/?page=10").get()

    val input = new File("src/main/scala/com/github/georgeii/67week.html")
    val doc: Document = Jsoup.parse(input, "UTF-8")

    val body: Element = doc.select("div.content").get(2)
    println(body)

    val header: String = body.select("h1").get(0).text()
    val weekNumber: Int = header.substring(1, header.indexOf(":")).toInt
    val headline: String = header.substring(header.indexOf(":") + 2, header.length)

    println(header)
    println(weekNumber)
    println(headline)


    val description: String = doc.select("div.links-blue").first().text()
    println(description)

    val apis = traverseWeekPageAndGetStoryApis(
      new mutable.ListBuffer[String],
      "https://blog.reedsy.com/creative-writing-prompts/contests/67/?page=18"
    )

    apis.foreach(x => println(x))
    println(apis.length)

    val urls = apis.map(api => "https://blog.reedsy.com" + api)

    val weekInfo = WeekInfo(weekNumber, headline, description, urls, LocalDateTime.now())

    val story: StoryModel = getStory("https://blog.reedsy.com/creative-writing-prompts/contests/67/submissions/41964/")
    println(story)
    val story2: StoryModel = getStory("https://blog.reedsy.com/creative-writing-prompts/contests/67/submissions/41964/")

    val stories: List[StoryModel] = weekInfo.storiesUrls.map(storyUrl => {
      Thread.sleep(500)
      getStory(storyUrl)
    }).toList

    println(stories)
    println(stories.getClass.toString)
    println(stories.length)

    (weekInfo, stories)
  }

  /**
   * Parses only one particular story by a given URL.
   * @param url
   * @return StoryModel
   */
  def getStory(url: String): StoryModel = {
    val doc: Document = Jsoup.connect(url).get()

    val author: String = doc.select("div.writing-prompts > section.row-thin > div.content-thin > " +
      "div.panel > div.panel-body > div.grid > div.cell > a > h4").first().text()

    val likesString: String = doc.select("div.writing-prompts > section.row-thin > div.content-thin > " +
      "div.grid > div.cell > p > span").first().text()
    val likesNumber: Int = likesString.substring(0, likesString.indexOf(" ")).toInt

    val commentsString: String = doc.select("div.writing-prompts > section.row-thin > div.content-thin > " +
      "div.grid > div.cell > p > a").first().text()
    val commentsNumber: Int = commentsString.substring(0, commentsString.indexOf(" ")).toInt

    val name: String = doc.select("div.writing-prompts > section.row-blue-dark > div.content-thin > h1")
      .first().text()

    val categories: List[String] = doc.select("div.writing-prompts > section.row-thin > div.content-thin > " +
      "p > a").eachText().asScala.toList

    val body: String = doc.select("article.submission-content > p").eachText().asScala.toList.mkString("\n")


    StoryModel(name, author, categories, body, likesNumber, commentsNumber, LocalDateTime.now())
  }

  /**
   * Primarily, traverses between pages to and includes extractStoryApis at every page.
   * @param apis - At the beginning, pass "new mutable.ListBuffer[String]" here.
   * @param url - At the beginning, page of your week here.
   * @return List of all the APIs of a given week.
   */
  @tailrec
  private def traverseWeekPageAndGetStoryApis(apis: mutable.ListBuffer[String], url: String): mutable.ListBuffer[String] = {
    val doc: Document = Jsoup.connect(url).get()

    println(doc.select("div#submissions-load > a").attr("href"))
    val nextPageApi: String = doc.select("div#submissions-load > a").attr("href")

    nextPageApi match {
      case x if x.isBlank => apis ++ extractStoryApis(doc)
      case _ =>
        // sleep to not flood the website with requests
        Thread.sleep(500)
        traverseWeekPageAndGetStoryApis(apis ++ extractStoryApis(doc), "https://blog.reedsy.com" + nextPageApi)
    }
  }

  /**
   * Extracts all the APIs (that means the domain of the website excluded) for stories which are contained by this page.
   * @param doc - Content of current page.
   * @return ListBuffer of APIs.
   */
  def extractStoryApis(doc: Document): mutable.ListBuffer[String] = {
    val submissionsContainerElement = doc.getElementById("submissions-container")
    val elements = submissionsContainerElement.select("div.cell-shrink > a")
    elements.forEach(x => println(x))

    val apis = new mutable.ListBuffer[String]
    elements.forEach(x => apis.append(x.attr("href")))
    println(apis)

    apis
  }

  /**
   * Gets the last finished week competition from the contains page.
   * @return Number of week.
   */
  def getLatestAvailableWeekNumber: String = {
    val url: String = "https://blog.reedsy.com/creative-writing-prompts/contests/"
    val doc: Document = Jsoup.connect(url).get()

    // this string contains the necessary number
    val numberAndHeadline: String = doc.select("h2.mimic-h3").get(1).text()

    // extracts the number
    numberAndHeadline.substring(1, numberAndHeadline.indexOf(":"))
  }

  def writeStoriesToDatabase(stories: List[StoryModel]): Unit = {
    val mongoClient: MongoClient = MongoClient("mongodb://localhost")
    val database: MongoDatabase = mongoClient.getDatabase("short-story-parser")
    val collection: MongoCollection[MongoDocument] = database.getCollection("stories")

    val documents = stories.map(x => MongoDocument(
      "name" -> x.name,
      "author" -> x.author,
      "categories" -> x.categories.mkString(", "),
      "body/story" -> x.body,
      "likesNumber" -> x.likesNumber,
      "commentsNumber" -> x.commentsNumber,
      "timestamp of scraping" -> x.timestamp.toString
    ))

    val observable: Observable[Completed] = collection.insertMany(documents)

    val promise = Promise[Boolean]
    observable.subscribe(new Observer[Completed] {

      override def onNext(result: Completed): Unit = println("Inserted")

      override def onError(e: Throwable): Unit = {
        println("Failed")
        promise.success(false)
      }

      override def onComplete(): Unit =  {
        println("Completed")
        promise.success(true)
      }
    })

    val future = promise.future
    Await.result(future, Duration(40, java.util.concurrent.TimeUnit.SECONDS))

    mongoClient.close()
  }

  def writeWeekInfoToDatabase(weekInfo: WeekInfo): Unit = {
    val mongoClient: MongoClient = MongoClient("mongodb://localhost")
    val database: MongoDatabase = mongoClient.getDatabase("short-story-parser")
    val collection: MongoCollection[MongoDocument] = database.getCollection("weeks")

    val document: MongoDocument = MongoDocument(
      "weekNumber" -> weekInfo.weekNumber,
      "headline" -> weekInfo.headline,
      "description" -> weekInfo.description,
      "storiesUrls" -> weekInfo.storiesUrls.mkString(", "),
      "timestamp" -> weekInfo.timestamp.toString
    )

    val observable: Observable[Completed] = collection.insertOne(document)

    val promise = Promise[Boolean]
    observable.subscribe(new Observer[Completed] {

      override def onNext(result: Completed): Unit = println("Inserted")

      override def onError(e: Throwable): Unit = {
        println("Failed")
        promise.success(false)
      }

      override def onComplete(): Unit =  {
        println("Completed")
        promise.success(true)
      }
    })

    val future = promise.future
    Await.result(future, Duration(10, java.util.concurrent.TimeUnit.SECONDS))

    mongoClient.close()
  }
}
