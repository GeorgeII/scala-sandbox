package com.george

import org.slf4j.LoggerFactory
import scala.sys.ShutdownHookThread

object DataGenerator {

  private val LOG = LoggerFactory.getLogger(getClass.getSimpleName)
  private val KAFKA = "localhost:9092"
  private val TOPIC = "tweets"

  def main(args: Array[String]): Unit = {
    val producer = new Producer(KAFKA, TOPIC)

    sys.addShutdownHook {
      LOG.info("Shutting down")
      producer.close()
    }

    producer.run()
  }

}
