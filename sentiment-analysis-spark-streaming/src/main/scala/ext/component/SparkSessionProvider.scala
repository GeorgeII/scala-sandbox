package ext.component

import org.apache.spark.sql.SparkSession

trait SparkSessionProvider {
  def sparkSession: SparkSession
}

class DefaultSparkSessionProvider(appName: String) extends SparkSessionProvider {
  override lazy val sparkSession: SparkSession = {
    val ss = SparkSession.builder
      .appName(appName)
      .config("spark.master", "local[4]")
      .getOrCreate()

    println(s"DefaultSparkSessionProvider($appName)")

    ss.sparkContext.getConf.getAll.foreach {
      case (key, value) => println(s"\t$key=$value")
    }

    ss.sparkContext.setLogLevel("ERROR")

    ss
  }
}

trait SparkSessionProviderComponent {
  def sparkSessionProvider: SparkSessionProvider
}
