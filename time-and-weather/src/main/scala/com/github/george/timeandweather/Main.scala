package com.github.george.timeandweather

import cats.effect.{ExitCode, IO, IOApp}

object Main extends IOApp {
  def run(args: List[String]) =
    TimeandweatherServer.stream[IO].compile.drain.as(ExitCode.Success)
}
