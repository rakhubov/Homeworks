import Main.contextShift
import cats.data.{NonEmptyList, Validated}
import cats.effect.{ExitCode, IO, IOApp, _}
import cats.implicits.toSemigroupKOps
import cats.syntax.all._
import doobie.implicits._
import doobie.implicits.legacy.localdate
import doobie.{Fragment, Fragments, Meta, Transactor}
import io.circe.generic.auto._
import org.http4s.Method.POST
import org.http4s.circe.CirceEntityCodec.{
  circeEntityDecoder,
  circeEntityEncoder
}
import cats.syntax.all._
import fs2.{Pipe, Pull, Stream}
import fs2.concurrent.{Queue, Topic}
import old.Author
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame
import org.http4s.{HttpRoutes, ParseFailure, QueryParamDecoder}

import java.time.{LocalDate, Year}
import java.util.UUID
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt

object WebSocketServer {

  def run(
      connectToDataBase: Transactor[IO]
  )(implicit concurent: ConcurrentEffect[IO], timer: Timer[IO]): IO[ExitCode] =
    for {
      chatTopic <- Topic[IO, String]("Hello!")
      _ <-
        BlazeServerBuilder[IO](ExecutionContext.global)
          .bindHttp(port = 8080, host = "localhost")
          .withHttpApp(httpRoute(connectToDataBase, chatTopic, concurent, timer))
          .serve
          .compile
          .drain
    } yield ExitCode.Success

  private def httpRoute(
      connectToDataBase: Transactor[IO],
      chatTopic: Topic[IO, String],
      concurent: ConcurrentEffect[IO],
      timer: Timer[IO]
  ) = {
    privateRoute(connectToDataBase)(concurent, timer) <+>
      sharedRoute(connectToDataBase, chatTopic)
  }.orNotFound
  //
  //

  private def privateRoute(
      connectToDataBase: Transactor[IO]
  )(implicit concurent: ConcurrentEffect[IO], timer: Timer[IO]) =
    HttpRoutes.of[IO] {

      case GET -> Root / "private" =>
        import ServerPrivateCommand.checkPrivatRequestion

        val checkMessage: Pipe[IO, WebSocketFrame, WebSocketFrame] = _.evalMap {
          case WebSocketFrame.Text(message, _) =>
            checkPrivatRequestion(message, connectToDataBase).map(response =>
              WebSocketFrame.Text(response)
            )
        }

        for {
          queue <- Queue.bounded[IO, WebSocketFrame](20)
          response <- WebSocketBuilder[IO].build(
            receive = queue.enqueue,
            send = queue.dequeue.through(checkMessage)
          )
        } yield response
    }
  //
  //
  //

  private def sharedRoute(
      connectToDataBase: Transactor[IO],
      chatTopic: Topic[IO, String]
  ): HttpRoutes[IO] = {
    HttpRoutes.of[IO] {

      case GET -> Root / "chat" =>
        import ServerSharedCommand.checkSharedRequestion

        WebSocketBuilder[IO].build(
          receive =
            chatTopic.publish.compose[Stream[IO, WebSocketFrame]](_.evalMap {
              case WebSocketFrame.Text(message, _) =>
                checkSharedRequestion(message, connectToDataBase)
            }),
          send = chatTopic.subscribe(20).map(WebSocketFrame.Text(_))
        )

    }
  }

}
