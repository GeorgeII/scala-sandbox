package com.github.george.timeandweather

import cats.effect.IO

trait Weather {
  def get(city: String): IO[Either[Weather.CurrentWeather, Weather.CurrentWeatherError]]
}

object Weather {

  def apply(implicit ev: Weather): Weather = ev

  final case class CurrentWeather(city: String, temperature: Double, windSpeed: Double)
  final case class CurrentWeatherError(error: String)

}
