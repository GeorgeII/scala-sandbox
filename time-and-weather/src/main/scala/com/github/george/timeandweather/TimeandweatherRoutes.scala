package com.github.george.timeandweather

import cats.effect.Sync
import cats.implicits._
import io.circe.Json
import org.http4s.HttpRoutes
import org.http4s.circe.jsonEncoder
import org.http4s.dsl.Http4sDsl

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

  def timeRoutes[F[_]: Sync](T: Times[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._

    HttpRoutes.of[F] {
      case GET -> Root / "time" / city =>
        for {
          timeOrErr <- T.get(city)
          resp      <- timeOrErr match {
            case Right(time) => Ok(time)
            case Left(error) => BadRequest(Json.obj("error" -> Json.fromString(error.e.getMessage)))
          }
        } yield resp
    }
  }
}