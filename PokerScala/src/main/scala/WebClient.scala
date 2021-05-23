import Main.contextShift
import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.implicits.catsSyntaxFlatMapOps
import org.http4s.client.jdkhttpclient.{
  JdkWSClient,
  WSConnectionHighLevel,
  WSFrame,
  WSRequest
}
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
    val id1 = clientPrivateResource.use { client =>
      for {
        _ <- client.send(WSFrame.Text("registration Lord1 2000"))
        receive <- client.receive
        idString = receive match {
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
    val id2 =
      clientSharedResource.use { client =>
        for {
          id <- id1
          _ <- client.send(WSFrame.Text(s"game $id 10 1001"))
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

          _ <- client.send(WSFrame.Text(s"start $id"))
          _ <-
            client.receiveStream
              .collectFirst {
                case WSFrame.Text(s, _) => s
              }
              .compile
              .string >>= printLine
        } yield id
      }
    //
    //
    val id3 = clientPrivateResource.use { client =>
      for {
        id <- id2
        _ <- client.send(WSFrame.Text(s"MyCard $id"))
        receive <- client.receive
        myCard = receive match {
          case Some(WSFrame.Text(message, _)) =>
            message
        }
        _ = println(myCard)
      } yield id
    }
    //
    //
    clientSharedResource.use { client =>
      for {
        id <- id3
        _ <- client.send(WSFrame.Text(s"fetchWinner $id"))
        _ <- (client.receiveStream
            .collectFirst {
              case WSFrame.Text(s, _) => s
            }
            .compile
            .string >>= printLine).foreverM
      } yield ExitCode.Success
    }
    //
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
        receive <- client.receive
        idString = receive match {
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
