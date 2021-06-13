package com.github.george.timeandweather

import cats.Applicative
import cats.implicits._
import cats.effect.Sync
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.generic.auto._
import org.http4s.{EntityDecoder, EntityEncoder}
import org.http4s.circe.{jsonEncoderOf, jsonOf}

import java.time._

trait Times[F[_]] {
  def get(city: String): F[Either[Times.CurrentTimeError, Times.CurrentTime]]
}

object Times {

  def apply[F[_]](implicit ev: Times[F]): Times[F] = ev

  final case class CurrentTime(city: String, time: LocalDateTime)
  final case class CurrentTimeError(exception: String) extends RuntimeException

  object Codecs {
//    implicit val decoderCurrentTime: Decoder[CurrentTime] = deriveDecoder[CurrentTime]
//    implicit def entityDecoderCurrentTime[F[_]: Sync]: EntityDecoder[F, CurrentTime] = jsonOf
//
//    implicit val encoderCurrentTime: Encoder[CurrentTime] = deriveEncoder[CurrentTime]
//    implicit def entityEncoderCurrentTime[F[_]: Applicative]: EntityEncoder[F, CurrentTime] = jsonEncoderOf


//    implicit val decoderCurrentTimeError: Decoder[CurrentTimeError] = deriveDecoder[CurrentTimeError]
//    implicit def entityDecoderCurrentTimeError[F[_]: Sync]: EntityDecoder[F, CurrentTimeError] = jsonOf
//
//    implicit val encoderCurrentTimeError: Encoder[CurrentTimeError] = deriveEncoder[CurrentTimeError]
//    implicit def entityEncoderCurrentTimeError[F[_]: Applicative]: EntityEncoder[F, CurrentTimeError] = jsonEncoderOf
  }

  def impl[F[_]: Applicative]: Times[F] = new Times[F] {
    override def get(city: String): F[Either[CurrentTimeError, CurrentTime]] = {
      getLocalDateTime(city).pure[F]
    }
  }

  private[this] def getLocalDateTime(city: String): Either[CurrentTimeError, CurrentTime] = {
    val region = city match {
      case "MOSCOW"      => Right("Europe/Moscow")
      case "LONDON"      => Right("Europe/London")
      case "SYDNEY"      => Right("Australia/Sydney")
      case "NEW-YORK"    => Right("America/New_York")
      case "LOS-ANGELES" => Right("America/Los_Angeles")
      case str @ _       => Left(CurrentTimeError(s"$str city is not supported yet."))
    }

    region.map { timeZone =>
      CurrentTime(
        city,
        LocalDateTime.now(ZoneId.of(timeZone))
      )
    }
  }
}
