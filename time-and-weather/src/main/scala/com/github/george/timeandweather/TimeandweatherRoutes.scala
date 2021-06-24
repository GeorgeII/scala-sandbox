package com.github.george.timeandweather

import cats.effect.IO
import fs2.Stream
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.HttpRoutes
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.circe.jsonEncoder
import org.http4s.dsl.Http4sDsl

import scala.concurrent.duration.DurationInt

object TimeandweatherRoutes {

  def jokeRoutes(J: Jokes[IO]): HttpRoutes[IO] = {
    val dsl = new Http4sDsl[IO]{}
    import dsl._
    HttpRoutes.of[IO] {
      case GET -> Root / "joke" =>
        for {
          joke <- J.get
          resp <- Ok(joke)
        } yield resp
    }
  }

  def helloWorldRoutes(H: HelloWorld[IO]): HttpRoutes[IO] = {
    val dsl = new Http4sDsl[IO]{}
    import dsl._

    HttpRoutes.of[IO] {
      case GET -> Root / "hello" / name =>
        for {
          greeting <- H.hello(HelloWorld.Name(name))
          resp <- Ok(greeting)
        } yield resp
    }
  }

  def weatherRoutes(weather: Times[IO]): HttpRoutes[IO] = {
    val dsl = new Http4sDsl[IO] {}
    import dsl._

    HttpRoutes.of[IO] {
      case GET -> Root / "weather" / city =>

    }
  }

  def timeRoutes(times: Times[IO]): HttpRoutes[IO] = {
    val dsl = new Http4sDsl[IO]{}
    import dsl._

    HttpRoutes.of[IO] {
      case GET -> Root / "time" / city =>
        for {
          timeOrErr <- times.get(city.toUpperCase)
          resp      <- timeOrErr match {
            case Right(time) => Ok(time.asJson)
            case Left(error) => BadRequest(error.asJson)
          }
        } yield resp
    }
  }

  def timeStreamingRoutes(times: Times[IO]): HttpRoutes[IO] = {
    val dsl = new Http4sDsl[IO]{}
    import dsl._

    HttpRoutes.of[IO] {
      case GET -> Root / "streaming" / city =>

        import Codecs.Time._

        val throttling    = Stream.awakeEvery[IO](1.second)
        val payloadStream = throttling.evalMap(_ => times.get(city.toUpperCase))

        Ok(payloadStream)
    }
  }
}