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

    val clientSharedResource = Resource
      .eval(IO(HttpClient.newHttpClient()))
      .flatMap(JdkWSClient[IO](_).connectHighLevel(WSRequest(uriChat)))

    //
    //
    val mayID = clientPrivateResource.use { client =>
      for {
        _ <- client.send(WSFrame.Text("registration Lord1 2000"))
        recive <- client.receive
        idString = recive match {
          case Some(WSFrame.Text(message, _)) =>
            message.split("\\s+").toList.last.trim
        }
        _ = println(idString)
        id = UUID.fromString(idString)
//        _ = Thread.sleep(1000)
      } yield id
    }
    //
    //
    clientSharedResource.use { client =>
      for {
        idUUID <- mayID
        _ <- client.send(WSFrame.Text(s"game $idUUID 10 1001"))
        _ <-
          client.receiveStream
            .collectFirst {
              case WSFrame.Text(s, _) => s
            }
            .compile
            .string
        _ <-
          client.receiveStream
            .collectFirst {
              case WSFrame.Text(s, _) => s
            }
            .compile
            .string >>= printLine

        _ <- client.send(WSFrame.Text(s"start $idUUID"))
        _ <-
          client.receiveStream
            .collectFirst {
              case WSFrame.Text(s, _) => s
            }
            .compile
            .string >>= printLine
        _ <-
          client.receiveStream
            .collectFirst {
              case WSFrame.Text(s, _) => s
            }
            .compile
            .string >>= printLine

      } yield ExitCode.Success
    }
  }
}

object WebSocketClient2 extends IOApp {
  private val uriPrivate = uri"ws://localhost:8080/private"
  private val uriChat = uri"ws://localhost:8080/chat"

  private def printLine(string: String = ""): IO[Unit] = IO(println(string))

  override def run(args: List[String]): IO[ExitCode] = {
    //  val mayID = Ref.of[IO, UUID](UUID.randomUUID())
    val clientPrivateResource = Resource
      .eval(IO(HttpClient.newHttpClient()))
      .flatMap(JdkWSClient[IO](_).connectHighLevel(WSRequest(uriPrivate)))

    val clientSharedResource = Resource
      .eval(IO(HttpClient.newHttpClient()))
      .flatMap(JdkWSClient[IO](_).connectHighLevel(WSRequest(uriChat)))

    //
    //
    val mayID = clientPrivateResource.use { client =>
      for {
        _ <- client.send(WSFrame.Text("registration Lord2 2000"))
        recive <- client.receive
        idString = recive match {
          case Some(WSFrame.Text(message, _)) =>
            message.split("\\s+").toList.last.trim
        }
        _ = println(idString)
        id = UUID.fromString(idString)
        //        _ = Thread.sleep(1000)
      } yield id
    }
    //
    //
    clientSharedResource.use { client =>
      for {
        idUUID <- mayID
        _ <- client.send(WSFrame.Text(s"dfgssdaaaaaaa"))
        _ <- client.send(WSFrame.Text(s"game $idUUID 10 1002"))

        _ <-
          client.receiveStream
            .collectFirst {
              case WSFrame.Text(s, _) => s
            }
            .compile
            .string
        _ <-
          client.receiveStream
            .collectFirst {
              case WSFrame.Text(s, _) => s
            }
            .compile
            .string >>= printLine
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
