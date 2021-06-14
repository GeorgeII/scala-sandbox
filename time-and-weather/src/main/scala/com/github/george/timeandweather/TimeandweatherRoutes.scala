package com.github.george.timeandweather

import cats.effect.{Sync, Timer}
import cats.implicits._
import fs2.{Pure, Stream}
import io.circe.Json
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.{HttpRoutes, Response}
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.circe.jsonEncoder
import org.http4s.dsl.Http4sDsl

import scala.concurrent.duration.DurationInt

object TimeandweatherRoutes {

  def jokeRoutes[F[_]: Sync](J: Jokes[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root / "joke" =>
        for {
          joke <- J.get
          resp <- Ok(joke)
        } yield resp
    }
  }

  def helloWorldRoutes[F[_]: Sync](H: HelloWorld[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root / "hello" / name =>
        for {
          greeting <- H.hello(HelloWorld.Name(name))
          resp <- Ok(greeting)
        } yield resp
    }
  }

  def timeRoutes[F[_]: Sync](times: Times[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._

    HttpRoutes.of[F] {
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

  def timeStreamingRoutes[F[_]: Sync : Timer](times: Times[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._

    HttpRoutes.of[F] {
      case GET -> Root / "streaming" / city =>

        val throttling = Stream.awakeEvery[F](1.second)

        for {
          timeOrErr <- times.get(city.toUpperCase)
          resp <- timeOrErr match {
            case Right(time) => Ok(throttling.map(_ => time.asJson))
            case Left(error) => BadRequest(throttling.map(_ => error.asJson))
          }
        } yield resp
    }
  }
}