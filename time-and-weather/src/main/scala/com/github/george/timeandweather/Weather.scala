package com.github.george.timeandweather

import cats.effect.IO

trait Weather {
  def get(city: String): IO[Either[Weather.CurrentWeather, Weather.CurrentWeatherError]]
}

object Weather {

  def apply(implicit ev: Weather): Weather = ev

  final case class CurrentWeather(city: String, temperature: Double, windSpeed: Double)
  final case class CurrentWeatherError(error: String)

  private val supportedCities = List("LONDON", "NEW-YORK", "MOSCOW", "LOS-ANGELES", "SYDNEY")

  def impl: Weather = new Weather {
    override def get(city: String): IO[Either[CurrentWeather, CurrentWeatherError]] = {
      getLocalWeather(city)
    }
  }

  private def getLocalWeather(city: String): IO[Either[CurrentWeather, CurrentWeatherError]] = {
    val resultCity =
      if (supportedCities.contains(city.toUpperCase))
        Left(CurrentWeatherError(s"$city city is not supported yet."))
      else
        Right(city.toUpperCase)

    // making an http request here. So, IO encompasses it because this is a side effect.
    IO {
      resultCity.map { city =>
        // TODO: an http request should be made here.
        // It must be checked whether it was successful and it it wasn't - return Left with a
        // convenient error message.
      }
    }
  }

}
