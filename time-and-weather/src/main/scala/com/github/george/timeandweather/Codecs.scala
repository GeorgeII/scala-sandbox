package com.github.george.timeandweather

import com.github.george.timeandweather.Times.{CurrentTime, CurrentTimeError}
import cats.effect.IO
import fs2.Stream
import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder
import io.circe.generic.auto._
import org.http4s.circe.streamJsonArrayEncoderOf
import org.http4s.dsl.Http4sDsl
import org.http4s.{EntityEncoder, HttpRoutes}

object Codecs {

  object Time {

    private implicit final val CirceEncoder: Encoder[CurrentTime] =
      deriveEncoder

    private implicit final val timeOrErrorCirceEncoder: Encoder[Either[CurrentTimeError, CurrentTime]] =
      Encoder.encodeEither[CurrentTimeError, CurrentTime](leftKey = "error", rightKey = "time")

    private implicit final val timeOrErrorEntityEncoder: EntityEncoder[IO, Stream[IO, Either[CurrentTimeError, CurrentTime]] =
      streamJsonArrayEncoderOf
  }

}
