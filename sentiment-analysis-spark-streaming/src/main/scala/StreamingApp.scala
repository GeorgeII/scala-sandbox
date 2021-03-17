import ext.component._
import org.apache.spark.streaming.{Duration, Seconds}
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import edu.stanford.nlp.ling.CoreAnnotations
import edu.stanford.nlp.neural.rnn.RNNCoreAnnotations
import edu.stanford.nlp.sentiment.SentimentCoreAnnotations

import java.util.Properties
import scala.collection.JavaConversions._
import scala.collection.mutable

trait DataDirectorySentimentAnalysis {
  this: StreamingContextProviderComponent =>

  private lazy val ssc = streamingContextProvider.streamingContext

//  val props = new Properties
//  props.setProperty("annotators", "tokenize, ssplit, pos, parse, sentiment")
//  val pipeline = new AdHocSerializableCoreNLP(props)

  def runDataDirectorySentimentAnalysis(dataDirectory: String, pipeline: StanfordCoreNLP): Unit = {
        val lines = ssc.textFileStream(dataDirectory)

    println(lines)


    // sentiment analysis

//    val props = new Properties
//    props.setProperty("annotators", "tokenize, ssplit, pos, parse, sentiment")
//    val pipeline = new AdHocSerializableCoreNLP(props)

    val predictions = lines
      .map(pipeline.process)
      .map(Main.predict)

    predictions.print()

//    val lines = ssc.textFileStream(dataDirectory)
//    val words = lines.flatMap(_.split(" "))
//    val wordCounts = words.map(x => (x, 1)).reduceByKey(_ + _)
//    wordCounts.print()

    ssc.start()
    ssc.awaitTermination()
  }
}

object DataDirectorySentimentAnalysisApp extends App
  with DataDirectorySentimentAnalysis
  with StreamingContextProviderComponent
  with SparkContextProviderComponent
{
  private lazy val defaultSparkContextProvider = new DefaultSparkContextProvider("SentimentAnalysisApp")

  override def sparkContextProvider: SparkContextProvider = defaultSparkContextProvider

  override def streamingContextProvider: StreamingContextProvider =
    new DefaultStreamingContextProvider
      with SparkContextProviderComponent {
    override def batchDuration: Duration = Seconds(10)
    override def sparkContextProvider: SparkContextProvider = defaultSparkContextProvider
  }

  val props = new Properties
  props.setProperty("annotators", "tokenize, ssplit, pos, parse, sentiment")
  val pipeline = new AdHocSerializableCoreNLP(props)

  runDataDirectorySentimentAnalysis("data", pipeline)
}

// To use map/flatMap/filter with Spark's DStream or RDD the functions and methods that are passed into them
// should be serializable because Spark delivers the code to the clusters containing the data to be processed.
// So I use this ad-hoc approach to achieve the required behavior.
class AdHocSerializableCoreNLP(props: Properties)
  extends StanfordCoreNLP(props)
    with Serializable

object Main {
  def predict(annotation: Annotation): mutable.Buffer[Int] = {
    for (sentence <- annotation.get(classOf[CoreAnnotations.SentencesAnnotation]))
      yield {
        val tree = sentence.get(classOf[SentimentCoreAnnotations.SentimentAnnotatedTree])
        RNNCoreAnnotations.getPredictedClass(tree)
      }
  }

  def main(args: Array[String]): Unit = {
    val str = "This is a very positive string! " +
      "I like this string so much! " +
      "But I hate this string. " +
      "And this string is absolutely terribly awful!" +
      "This is a good one"

    val props = new Properties
    props.setProperty("annotators", "tokenize, ssplit, pos, parse, sentiment")
    val pipeline = new AdHocSerializableCoreNLP(props)

    val annotation = pipeline.process(str)
//    val res = for (sentence <- annotation.get(classOf[CoreAnnotations.SentencesAnnotation]))
//      yield {
//        val tree = sentence.get(classOf[SentimentCoreAnnotations.SentimentAnnotatedTree])
//        RNNCoreAnnotations.getPredictedClass(tree)
//      }
    val res = predict(annotation)
    res.foreach(println)
  }
}
