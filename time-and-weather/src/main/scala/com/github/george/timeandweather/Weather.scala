package com.github.george.timeandweather

import cats.data.EitherT
import cats.effect.IO

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
        EitherT.left(IO(CurrentWeatherError(s"$city city is not supported yet.")))
      else
        EitherT.right(IO(city.toUpperCase))

    for {
      eitherCity <- resultCity
      rightCity <- getByCity(eitherCity)
    } yield rightCity
  }

  // making an http request here. So, IO encompasses it because this is a side effect.
  private def getByCity(city: String): EitherT[IO, CurrentWeatherError, CurrentWeather] = {
    
  }

}
