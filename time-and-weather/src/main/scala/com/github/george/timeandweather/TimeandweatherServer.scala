package com.github.george.timeandweather

import cats.effect.{ConcurrentEffect, IO, Timer}
import cats.implicits._
import fs2.Stream
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.Logger

import scala.concurrent.ExecutionContext.global
import com.github.george.timeandweather.Codecs.Time.cs

object TimeandweatherServer {

  def stream[F[_]: ConcurrentEffect](implicit T: Timer[IO]): Stream[IO, Nothing] = {
    for {
      client <- BlazeClientBuilder[IO](global).stream
      helloWorldAlg = HelloWorld.impl[IO]
      jokeAlg = Jokes.impl[IO](client)
      timeAlg = Times.impl[IO]

      // Combine Service Routes into an HttpApp.
      // Can also be done via a Router if you
      // want to extract a segments not checked
      // in the underlying routes.
      httpApp = (
        TimeandweatherRoutes.helloWorldRoutes(helloWorldAlg) <+>
        TimeandweatherRoutes.jokeRoutes(jokeAlg)             <+>
        TimeandweatherRoutes.timeRoutes(timeAlg)             <+>
        TimeandweatherRoutes.timeStreamingRoutes(timeAlg)
      ).orNotFound

      // With Middlewares in place
      finalHttpApp = Logger.httpApp(logHeaders = true, logBody = true)(httpApp)

      exitCode <- BlazeServerBuilder[IO](global)
        .bindHttp(8080, "0.0.0.0")
        .withHttpApp(finalHttpApp)
        .serve
    } yield exitCode
  }.drain
}
