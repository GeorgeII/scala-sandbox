package com.github.george.timeandweather

import cats.effect.{ContextShift, IO, Timer}
import com.github.george.timeandweather.Times.{CurrentTime, CurrentTimeError}
import com.github.george.timeandweather.Weather.{CurrentWeather, CurrentWeatherError}
import fs2.Stream
import io.circe.{Encoder, Json}
import io.circe.generic.auto._
import io.circe.generic.semiauto.deriveEncoder
import org.http4s.EntityEncoder
import org.http4s.circe.streamJsonArrayEncoderOf

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global

object Codecs {

  object Time {

    val executionContext: ExecutionContext = implicitly[ExecutionContext]
    implicit val timerEncoder: Timer[IO] = IO.timer(executionContext)
    implicit val cs: ContextShift[IO] = IO.contextShift(executionContext)

    implicit val circeEncoder: Encoder[CurrentTime] = deriveEncoder

    implicit val timeOrErrorCirceEncoder: Encoder[Either[CurrentTimeError, CurrentTime]] =
      Encoder.encodeEither[CurrentTimeError, CurrentTime](leftKey = "error", rightKey = "currentTime")

    implicit val timeOrErrorEntityEncoder: EntityEncoder[IO, Stream[IO, Either[CurrentTimeError, CurrentTime]]] =
      streamJsonArrayEncoderOf

  }

  object Weather {
    implicit val currentWeatherEncoder: Encoder[CurrentWeather] = new Encoder[CurrentWeather] {
      override def apply(a: CurrentWeather): Json = Json.fromString(a.details)
    }

    implicit val weatherErrorEncoder: Encoder[CurrentWeatherError] = new Encoder[CurrentWeatherError] {
      override def apply(a: CurrentWeatherError): Json = Json.fromString(a.error)
    }

    implicit val weatherEitherCirceEncoder: Encoder[Either[CurrentWeatherError, CurrentWeather]] =
      Encoder.encodeEither[CurrentWeatherError, CurrentWeather](leftKey = "error", rightKey = "currentWeather")
  }

}
