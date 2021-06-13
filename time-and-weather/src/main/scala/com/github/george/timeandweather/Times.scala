package com.github.george.timeandweather

import cats.Applicative
import cats.implicits._

import java.time._

trait Times[F[_]] {
  def get(city: String): F[Either[Times.CurrentTimeError, Times.CurrentTime]]
}

object Times {

  def apply[F[_]](implicit ev: Times[F]): Times[F] = ev

  final case class CurrentTime(city: String, time: LocalDateTime)
  final case class CurrentTimeError(error: String) extends RuntimeException


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
