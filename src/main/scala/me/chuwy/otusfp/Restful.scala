package me.chuwy.otusfp

import scala.concurrent.ExecutionContext.global
import cats.data.{Kleisli, OptionT, ReaderT}
import cats.effect.IO.{IOCont, Uncancelable}
import cats.effect.{IO, _}
import io.circe.{Decoder, DecodingFailure, Encoder, Json, JsonObject}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import me.chuwy.otusfp.Restful.{counter, httpApp}
import org.http4s.{AuthedRequest, AuthedRoutes, EntityDecoder, EntityEncoder, Header, HttpApp, HttpRoutes, Method, Request, Status}
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.circe.{jsonDecoder, jsonEncoderOf, jsonOf}
import org.http4s.client.Client
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.{AuthMiddleware, Router}
import org.http4s.server.websocket.WebSocketBuilder
import org.typelevel.ci.CIString

object Restful {

  val websocket = WebSocketBuilder[IO].build(x => x)

  type RIO[Env, A] = Kleisli[IO, Env, A]

  val r1 = Kleisli { (s: String) => IO.pure(s.length) }
  val r2 = Kleisli { (s: Int) => IO.pure(s) }
  val r3 = Kleisli { (s: Int) => IO.pure(s.toLong) }

//  val result: Kleisli[IO, String, Int] = for {
//    l1 <- r1
//    l2 <- r2
//  } yield l1 + l2

  def f1: Int => String = ???
  def f2: String => Long = ???

  def run1 = f1.andThen(f2)
  def run2 = r1.andThen(r2).andThen(r3)
  def run3 = r1.run("hello")

  case class User(name: String, id: Int)

  object User {
    implicit val userDecoder: Decoder[User] =
      Decoder.instance { cur =>
        for {
          jsonObject <- cur.as[JsonObject]
          nameOpt <- jsonObject.apply("name").toRight(DecodingFailure("Key name doesn't exist", cur.history))
          name <- nameOpt.as[String]
          idOpt <- jsonObject.apply("id").toRight(DecodingFailure("Key id doesn't exist", cur.history))
          id <- idOpt.as[Option[Int]]
        } yield User(name, id.get)
      }

    implicit val userEncoder: Encoder[User] =
      deriveEncoder[User]

    implicit def userEntityDecoder[F[_]: Concurrent]: EntityDecoder[F, User] =
      jsonOf[F, User]

    implicit def userEntityEncoder[F[_]: Concurrent]: EntityEncoder[F, User] =
      jsonEncoderOf[F, User]
  }

  def authUser =
    Kleisli { (req: Request[IO]) =>
      req.headers.get(CIString("user")) match {
        case Some(userHeaders) =>
          OptionT.liftF(IO.pure(User(userHeaders.head.value, 1)))
        case None =>
          OptionT.liftF(IO.pure(User("anon", 0)))
      }
    }


  def addHeader(businessLogic: HttpRoutes[IO]): HttpRoutes[IO] = {
    val header: Header.ToRaw = "X-Otus" -> "webinar"
    Kleisli { (req: Request[IO]) =>
      businessLogic.map {         // What's the difference with service(req).map ...
        case Status.Successful(resp) =>
          resp.putHeaders(header)
        case resp => resp
      }.apply(req)
    }
  }

  val serviceOne: HttpRoutes[IO] =
    HttpRoutes.of {
      case GET -> Root / "hello" / name =>
        websocket
      case req @ POST -> Root / "hello" =>
        req.as[User].flatMap { user =>
          Ok(s"Hello, $user")
        }
    }

  val authMiddleware = AuthMiddleware(authUser)

  def authedService: AuthedRoutes[User, IO] =
    AuthedRoutes.of {
      case GET -> Root / "hello" / name as user =>
        user match {
          case User(_, 0) => Ok("You're anonymous")
          case User(userName, _) => Ok(s"You're ${userName} accessing hello/${name}")
        }
    }

  val counter: Resource[IO, Ref[IO, Int]] = Resource.eval(Ref.of[IO, Int](0))
  type ServerContext = Ref[IO, Int]

  def counterService(counter: ServerContext): HttpRoutes[IO] = {
    HttpRoutes.of {
      case GET -> Root => {
        for {
          newCount <- counter.updateAndGet(x => x + 1)
          resp <- Ok(s"""{"counter": $newCount}""")
        } yield resp
      }
    }
  }

  def httpApp: HttpApp[IO] = {
    Router(
      "/" -> addHeader(serviceOne),
      "/auth" -> authMiddleware(authedService)
    ).orNotFound
  }

  def httpApp(counter : ServerContext): HttpApp[IO] = {
    Router(
      "/" -> addHeader(serviceOne),
      "/counter" -> counterService(counter),
      "/auth" -> authMiddleware(authedService)
    ).orNotFound
  }

  def builder(counter: ServerContext) =
    BlazeServerBuilder[IO](global)
      .bindHttp(port = 8080, host = "localhost")
      .withHttpApp(httpApp(counter))

  //tests

  import org.http4s.client.Client
}


object Test extends IOApp.Simple {

  private def testCounter() = {
    val request: Request[IO] = Request(uri = uri"/counter")
    val request2: Request[IO] = Request(uri = uri"/counter")
    counter.use {
      c =>
        val client = Client.fromHttpApp(httpApp(c))
        for {
          resp <- client.expect[Json](request)
          v <- IO.pure(assert(resp.noSpaces == """{"counter":1}"""))
          resp2 <- client.expect[Json](request2)
          res <- IO.pure(assert(resp2.noSpaces == """{"counter":2}"""))
        } yield res
    }

  }

  def run: IO[Unit] = {
    testCounter().flatMap(_ => IO.print("testCounter passed"))
      .onError(e => IO.print(s"testCounter failed: ${e.getMessage}; ${e.getStackTrace.map(_.toString).mkString(";\n")}"))
  }
}
