package com.george

import org.apache.kafka.clients.producer.{KafkaProducer, ProducerConfig, ProducerRecord}
import org.apache.kafka.common.serialization.LongSerializer

import java.time.{LocalDateTime, ZoneOffset}
import java.util.Properties
import scala.util.Random

class Producer(brokers: String, topic: String)
  extends Runnable
    with AutoCloseable {

  private val tweetsFile = "data/trump_insult_tweets_2014_to_2021.csv"
  private val rnd = Random
  private var isRunning = true

  type Tweet = String

  private def pickRandomTweetWithDelay(tweets: Vector[Tweet]): Tweet = {
    Thread.sleep(rnd.nextInt(1000))

    tweets(rnd.nextInt(tweets.length))
  }

  private def getTweetsFromCsv: Vector[Tweet] = {
    val bufferedSource = io.Source.fromFile(tweetsFile)

    val tweets = for {
      line <- bufferedSource.getLines().toVector
      tweet <- parseCsvLine(line)
    } yield tweet

    bufferedSource.close()

    tweets
  }

  def parseCsvLine(line: String): Option[Tweet] = {
    val columns = line.split(",")

    if (columns.length >= 5) Option(columns(4)) else None
  }

  override def run(): Unit = {
    val producer = new KafkaProducer[Long, Tweet](getProperties)
    val tweets = getTweetsFromCsv

    var counter = 0L

    while (isRunning) {
      val randomTweet = pickRandomTweetWithDelay(tweets)
      val millis = LocalDateTime.now().atZone(ZoneOffset.UTC).toInstant.toEpochMilli

      val record: ProducerRecord[Long, Tweet] =
        new ProducerRecord(topic, null, millis, counter, randomTweet)
      producer.send(record)

      counter += 1
    }

    producer.close()
  }

  override def close(): Unit = {
    isRunning = false
  }

  private def getProperties = {
    val props = new Properties
    props.put(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, brokers)
    props.put(ProducerConfig.ACKS_CONFIG, "all")
    //props.put(ProducerConfig.RETRIES_CONFIG, 0)
    props.put(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, classOf[LongSerializer])
    props.put(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, "org.apache.kafka.common.serialization.StringSerializer")

    props
  }
}
