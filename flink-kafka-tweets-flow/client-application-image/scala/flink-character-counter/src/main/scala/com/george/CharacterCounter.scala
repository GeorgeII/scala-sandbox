package com.george

import org.apache.flink.api.common.serialization.SimpleStringSchema
import org.apache.flink.api.common.typeinfo.TypeInformation
import org.apache.flink.api.java.utils.ParameterTool
import org.apache.kafka.clients.consumer.ConsumerConfig
import org.apache.kafka.clients.producer.ProducerConfig
import org.apache.flink.streaming.api.scala.{StreamExecutionEnvironment, createTypeInformation}
import org.apache.flink.streaming.connectors.kafka.{FlinkKafkaConsumer, FlinkKafkaProducer}

import java.util.Properties
import java.util.concurrent.TimeUnit

object CharacterCounter {

  val CHECKPOINTING_OPTION = "checkpointing"
  val EVENT_TIME_OPTION = "event-time"
  val BACKPRESSURE_OPTION = "backpressure"
  val OPERATOR_CHAINING_OPTION = "chaining"


  def main(args: Array[String]): Unit = {

    val params = ParameterTool.fromArgs(args)
    val env = StreamExecutionEnvironment.getExecutionEnvironment;

    configureEnvironment(params, env)

    val inflictBackpressure = params.has(BACKPRESSURE_OPTION)
    val inputTopic = params.get("input-topic", "input")
    val outputTopic = params.get("output-topic", "output")
    val brokers = params.get("bootstrap.servers", "localhost:9092")
    val kafkaProps = new Properties
    kafkaProps.setProperty(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, brokers)
    kafkaProps.setProperty(ConsumerConfig.GROUP_ID_CONFIG, "tweet-character-count")

    implicit val typeInfo = TypeInformation.of(classOf[String])

    val kafkaConsumer = new FlinkKafkaConsumer[String](
      "tweets",
      new SimpleStringSchema,
      kafkaProps
    )

    val kafkaProducer = new FlinkKafkaProducer[String](
      "processed-tweets",
      new SimpleStringSchema,
      kafkaProps
    )

    val stream = env
      .addSource(kafkaConsumer)
      .map(_.length.toString)
      .addSink(kafkaProducer)

    env.execute()
  }

  private def configureEnvironment(params: ParameterTool, env: StreamExecutionEnvironment): Unit = {
    val checkpointingEnabled = params.has(CHECKPOINTING_OPTION)
    val eventTimeSemantics = params.has(EVENT_TIME_OPTION)
    val enableChaining = params.has(OPERATOR_CHAINING_OPTION)

    if (checkpointingEnabled)
      env.enableCheckpointing(1000)

    if (!enableChaining)
      //disabling Operator chaining to make it easier to follow the Job in the WebUI
      env.disableOperatorChaining
  }
}
