import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.syntax.all._
import fs2.{Pipe, Pull, Stream}
import fs2.concurrent.{Queue, Topic}
import org.http4s._
import org.http4s.client.jdkhttpclient.{
  JdkWSClient,
  WSConnectionHighLevel,
  WSFrame,
  WSRequest
}
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame

import java.net.http.HttpClient
import scala.concurrent.ExecutionContext
import scala.util.Random
import scala.concurrent.duration._

object test2 extends IOApp {

  def periodicReader(ref: Ref[IO, Int]): IO[Unit] =
    IO.sleep(1.second) >> ref.get.flatMap(i =>
      IO(println(s"Current value is $i"))
    ) >> periodicReader(ref)

  def periodicIncrementer(ref: Ref[IO, Int]): IO[Unit] =
    IO.sleep(750.millis) >> ref.update(_ + 1) >> periodicIncrementer(ref)

  override def run(args: List[String]): IO[ExitCode] = {
    val ref = Ref[IO].of(0)

    for {
      _ <- ref.flatMap(_.update(_ + 42))
      x <- ref.flatMap(_.get)
      _ <- IO(println(s"The value is $x"))
    } yield ExitCode.Success
  }
}
