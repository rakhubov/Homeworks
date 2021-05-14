import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.implicits.catsSyntaxFlatMapOps
import org.http4s.client.jdkhttpclient.{JdkWSClient, WSFrame, WSRequest}
import org.http4s.implicits.http4sLiteralsSyntax

import java.net.http.HttpClient

object WebSocketClient extends IOApp {
  private val uri = uri"ws://localhost:8080/private"

  private def printLine(string: String = ""): IO[Unit] = IO(println(string))

  override def run(args: List[String]): IO[ExitCode] = {
    val clientResource = Resource
      .eval(IO(HttpClient.newHttpClient()))
      .flatMap(JdkWSClient[IO](_).connectHighLevel(WSRequest(uri)))

    clientResource.use { client =>
      for {
        _ <- client.send(WSFrame.Text("range"))
        _ <- client.send(WSFrame.Text("hello2"))
        _ <- client.send(WSFrame.Text("hello3"))
        _ <- (client.receiveStream
            .collectFirst {
              case WSFrame.Text(s, _) => s
            }
            .compile
            .string >>= printLine).foreverM
      } yield ExitCode.Success
    }
  }
}

object WebSocketClient2 extends IOApp {
  private val uri = uri"ws://localhost:8080/chat"
  private val name = "Lord"
  private def printLine(string: String = ""): IO[Unit] = IO(println(string))

  override def run(args: List[String]): IO[ExitCode] = {
    val clientResource = Resource
      .eval(IO(HttpClient.newHttpClient()))
      .flatMap(JdkWSClient[IO](_).connectHighLevel(WSRequest(uri)))

    clientResource.use { client =>
      for {
        _ <- client.send(WSFrame.Text(s"$name"))
        _ <- client.send(WSFrame.Text("range2"))
        _ <- client.send(WSFrame.Text("range3"))
        _ <-
          client.receiveStream
            .collectFirst {
              case WSFrame.Text(s, _) => s
            }
            .compile
            .string
        //
        _ <- (client.receiveStream
            .collectFirst {
              case WSFrame.Text(s, _) => s
            }
            .compile
            .string >>= printLine).foreverM
      } yield ExitCode.Success
    }
  }
}
