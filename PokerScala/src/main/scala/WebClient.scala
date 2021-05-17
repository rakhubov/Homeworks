import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.implicits.catsSyntaxFlatMapOps
import org.http4s.client.jdkhttpclient.{JdkWSClient, WSFrame, WSRequest}
import org.http4s.implicits.http4sLiteralsSyntax

import java.net.http.HttpClient
import java.util.UUID

object WebSocketClient extends IOApp {
  private val uriPrivate = uri"ws://localhost:8080/private"
  private val uriChat = uri"ws://localhost:8080/chat"

  private def printLine(string: String = ""): IO[Unit] = IO(println(string))

  override def run(args: List[String]): IO[ExitCode] = {
    //  val mayID = Ref.of[IO, UUID](UUID.randomUUID())
    val clientPrivateResource = Resource
      .eval(IO(HttpClient.newHttpClient()))
      .flatMap(JdkWSClient[IO](_).connectHighLevel(WSRequest(uriPrivate)))

    val mayID = clientPrivateResource.use { client =>
      for {
        _ <- client.send(WSFrame.Text("registration Lord 2000"))
        //       _ <- client.send(WSFrame.Text("1"))
//        _ <- client.send(WSFrame.Text("2"))
//        _ <- (client.receiveStream
//            .collectFirst {
//              case WSFrame.Text(s, _) => s
//            }
//            .compile
//            .string >>= printLine)
        recive <- client.receive
        idString = recive match {
          case Some(WSFrame.Text(message, _)) =>
            message.split("\\s+").toList.last.trim
        }
        _ = println(idString)
        //       _ <- IO(idString) >>= printLine
        id = UUID.fromString(idString)
        _ = Thread.sleep(1000)
      } yield id
    }
    //
    //
    val clientSharedResource = Resource
      .eval(IO(HttpClient.newHttpClient()))
      .flatMap(JdkWSClient[IO](_).connectHighLevel(WSRequest(uriChat)))

    clientSharedResource.use { client =>
      for {
        idUUID <- mayID
        id2 = UUID.fromString("ef8737a8-7fe1-4cfe-9739-263d70937a0d")
        _ <- client.send(WSFrame.Text(s"dfgssdaaaaaaa"))
        _ <- client.send(WSFrame.Text(s"game $idUUID 10 1000"))
//        _ <- client.send(WSFrame.Text("hello2"))
//        _ <- client.send(WSFrame.Text("hello3"))
        _ <- (client.receiveStream
            .collectFirst {
              case WSFrame.Text(s, _) => s
            }
            .compile
            .string >>= printLine) //.foreverM
        _ <- (client.receiveStream
            .collectFirst {
              case WSFrame.Text(s, _) => s
            }
            .compile
            .string >>= printLine)
        _ <- (client.receiveStream
            .collectFirst {
              case WSFrame.Text(s, _) => s
            }
            .compile
            .string >>= printLine)

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
