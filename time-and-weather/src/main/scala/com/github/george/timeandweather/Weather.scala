package com.github.george.timeandweather

import cats.data.EitherT
import cats.effect.IO
import org.http4s.client.blaze._
import org.http4s.client._
import Codecs.Time._
import org.http4s.Uri
import org.http4s.implicits.http4sLiteralsSyntax
import pureconfig._
import pureconfig.error.ConfigReaderFailures
import pureconfig.generic.auto._

import scala.concurrent.ExecutionContext.global


trait Weather {
  def get(city: String): EitherT[IO, Weather.CurrentWeatherError, Weather.CurrentWeather]
}

object Weather {

  def apply(implicit ev: Weather): Weather = ev

  final case class CurrentWeather(details: String)
  final case class CurrentWeatherError(error: String)

  private val supportedCities = List("LONDON", "NEW-YORK", "MOSCOW", "LOS-ANGELES", "SYDNEY")

  def impl: Weather = new Weather {
    override def get(city: String): EitherT[IO, CurrentWeatherError, CurrentWeather] = {
      getLocalWeather(city)
    }
  }

  private def getLocalWeather(city: String): EitherT[IO, CurrentWeatherError, CurrentWeather] = {
    val resultCity: Either[CurrentWeatherError, String] =
      if (supportedCities.contains(city.toUpperCase))
        Right(city.toUpperCase)
      else
        Left(CurrentWeatherError(s"$city city is not supported yet."))

    for {
      rightCity <- EitherT.fromEither[IO](resultCity)
      weather   <- getByCity(rightCity)
    } yield weather
  }

  // making an http request here. So, IO encompasses it because this is a side effect.
  private def getByCity(city: String): EitherT[IO, CurrentWeatherError, CurrentWeather] = {

    case class OpenWeatherMap(openWeatherMap: OpenWeatherMapDetails)
    case class OpenWeatherMapDetails(apiKey: String)

    val conf = EitherT.fromEither[IO](ConfigSource.default.load[OpenWeatherMap])

    val url: EitherT[IO, ConfigReaderFailures, Uri] =
      for {
        openWeatherMap <- conf
      } yield {
        val baseUrl  = uri"https://api.openweathermap.org/data/2.5/weather"
        val wholeUrl =
          baseUrl
            .withQueryParam("q", city)
            .withQueryParam("appid", openWeatherMap.openWeatherMap.apiKey)

        wholeUrl
      }

    val request = BlazeClientBuilder[IO](global).resource.use { client =>
      val response = url.semiflatMap( urlString => client.expect[String](urlString))

      response.value
    }

    val response =
      EitherT(request)
        .bimap(
          configReaderFailure => CurrentWeatherError(configReaderFailure.toString),
          weatherDetails      => CurrentWeather(weatherDetails)
        )

    response
  }
}
