package me.chuwy.otusfp

import scala.concurrent.duration._
import cats.effect.{IO, IOApp}
import me.chuwy.otusfp.Restful.{User, counter}

object Main extends IOApp.Simple {
  import io.circe.literal._
  import io.circe.syntax._
  import io.circe.parser.parse


  def run: IO[Unit] = {
    val exampleJson = json"""{"name": "Bob", "id": 42}"""
    val result = User("Alice", 1).asJson

    val server = {
      for {
        count <- counter
        s <- Restful.builder(count).resource
      } yield s
    }

    for {
      t <- server.use(_ => IO.never).start
      _ <- IO.sleep(2.seconds)
      res <- Last.requestWithUser *> IO.never
    } yield res
  }
}
