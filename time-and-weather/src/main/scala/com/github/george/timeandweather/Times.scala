package com.github.george.timeandweather

import cats.Applicative
import cats.implicits._
import cats.effect.Sync
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import org.http4s.{EntityDecoder, EntityEncoder}
import org.http4s.circe.{jsonEncoderOf, jsonOf}

import java.time._

trait Times[F[_]] {
  def get(city: String): F[Either[Times.CurrentTimeError, Times.CurrentTime]]
}

object Times {

  def apply[F[_]](implicit ev: Times[F]): Times[F] = ev

  final case class CurrentTime(city: String, time: LocalDateTime)

  object CurrentTime {
    implicit val decoderCurrentTime: Decoder[CurrentTime] = deriveDecoder[CurrentTime]
    implicit def entityDecoderCurrentTime[F[_]: Sync]: EntityDecoder[F, CurrentTime] = jsonOf
    implicit val encoderCurrentTime: Encoder[CurrentTime] = deriveEncoder[CurrentTime]
    implicit def entityEncoderCurrentTime[F[_]: Applicative]: EntityEncoder[F, CurrentTime] = jsonEncoderOf
  }

  final case class CurrentTimeError(e: Throwable) extends RuntimeException

  def impl[F[_]: Applicative]: Times[F] = new Times[F] {
    override def get(city: String): F[Either[CurrentTimeError, CurrentTime]] = {
      getLocalDateTime(city).pure[F]
    }
  }

  private[this] def getLocalDateTime(city: String): Either[CurrentTimeError, CurrentTime] = {
    val region = city match {
      case "Moscow"      => Right("Europe/Moscow")
      case "London"      => Right("Europe/London")
      case "Sydney"      => Right("Australia/Sydney")
      case "New-York"    => Right("America/New_York")
      case "Los-angeles" => Right("America/Los_Angeles")
      case str @ _ => Left(CurrentTimeError(new NoSuchElementException(s"$str city is not supported.")))
    }

    region.map { timeZone =>
      CurrentTime(
        city,
        LocalDateTime.now(ZoneId.of(timeZone))
      )
    }
  }
}
