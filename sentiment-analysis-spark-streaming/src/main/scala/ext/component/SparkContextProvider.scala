package ext.component

import org.apache.spark.{SparkConf, SparkContext}

trait SparkContextProvider {
  def sparkContext: SparkContext
}

class DefaultSparkContextProvider(appName: String) extends SparkContextProvider {
  override lazy val sparkContext: SparkContext = {
    val sparkConf = new SparkConf()
      .setMaster("local[4]")
      .setAppName(appName)

    new SparkContext(sparkConf)
  }
}

trait SparkContextProviderComponent {
  def sparkContextProvider: SparkContextProvider
}
