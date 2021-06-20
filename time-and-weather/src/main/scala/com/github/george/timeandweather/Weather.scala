package com.github.george.timeandweather

import cats.data.EitherT
import cats.effect.IO
import org.http4s.client.blaze._
import org.http4s.client._
import Codecs.Time._

import scala.concurrent.ExecutionContext.global


trait Weather {
  def get(city: String): IO[Either[Weather.CurrentWeatherError, Weather.CurrentWeather]]
}

object Weather {

  def apply(implicit ev: Weather): Weather = ev

  final case class CurrentWeather(city: String, temperature: Double, windSpeed: Double)
  final case class CurrentWeatherError(error: String)

  private val supportedCities = List("LONDON", "NEW-YORK", "MOSCOW", "LOS-ANGELES", "SYDNEY")

  def impl: Weather = new Weather {
    override def get(city: String): EitherT[IO, CurrentWeatherError, CurrentWeather] = {
      getLocalWeather(city)
    }
  }

  private def getLocalWeather(city: String): EitherT[IO, CurrentWeatherError, CurrentWeather] = {
    val resultCity: EitherT[IO, CurrentWeatherError, String] =
      if (supportedCities.contains(city.toUpperCase))
        EitherT.right(IO(city.toUpperCase))
      else
        EitherT.left(IO(CurrentWeatherError(s"$city city is not supported yet.")))

    for {
      rightCity <- resultCity
      weather <- getByCity(rightCity)
    } yield weather
  }

  // making an http request here. So, IO encompasses it because this is a side effect.
  private def getByCity(city: String): EitherT[IO, CurrentWeatherError, CurrentWeather] = {
    // TODO: read apiKey from the config
    val apiKey = ""
    val url = s"api.openweathermap.org/data/2.5/weather?q=$city&appid=$apiKey"

    val request = BlazeClientBuilder[IO](global).resource.use { client =>
      client.expect[String](url)
    }

    // TODO: figure out where to run unsafeRunSync as it's better to be deferred until the end.
    request.unsafeRunSync()
  }

}
