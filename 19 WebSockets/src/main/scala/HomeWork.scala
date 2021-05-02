import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.syntax.all._
import fs2.Pipe
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
import Protoc._
import scala.util.Random

object Protoc {
  val randomNomberForUser = scala.collection.mutable.Map[String, Int]()
  val great = "Great"
  val low = "Low"
  val equal = "Equal"
}

object WebSocketServer extends IOApp {

  private val gameRoutes = HttpRoutes.of[IO] {

    case GET -> Root / "game" =>
      val respons: Pipe[IO, WebSocketFrame, WebSocketFrame] =
        _.collect {
          case WebSocketFrame.Text(message, _) =>
            message.split("\\s+").toList match {
              case "range" :: name :: minRange :: maxRange :: Nil =>
                minRange.toIntOption :: maxRange.toIntOption :: Nil match {
                  case Some(min) :: Some(max) :: Nil =>
                    randomNomberForUser += (name -> Random
                      .between(min, max));
                    WebSocketFrame.Text(
                      s"Hello, $name! The server guessed a " +
                        s"number in the range from $min to $max. Can you guess? \n"
                    )
                }
              case "guess" :: name :: guess :: Nil =>
                guess.toIntOption match {
                  case Some(guessInt) => {
                    if ((randomNomberForUser get name) > Option(guessInt))
                      WebSocketFrame.Text(great)
                    else if ((randomNomberForUser get name) < Option(guessInt))
                      WebSocketFrame.Text(low)
                    else WebSocketFrame.Text(equal)
                  }
                }
              case _ =>
                WebSocketFrame.Text(
                  s"Response is invalid $message \n"
                )
            }
          case message =>
            WebSocketFrame.Text(
              s"Response is invalid $message \n"
            )
        }

      for {
        queue <- Queue.unbounded[IO, WebSocketFrame]
        response <- WebSocketBuilder[IO].build(
          receive = queue.enqueue,
          send = queue.dequeue.through(respons)
        )
      } yield response
  }

  private def httpApp(chatTopic: Topic[IO, String]) = {
    gameRoutes
  }.orNotFound

  override def run(args: List[String]): IO[ExitCode] =
    for {
      chatTopic <- Topic[IO, String]("Hello!")
      _ <-
        BlazeServerBuilder[IO](ExecutionContext.global)
          .bindHttp(port = 9002, host = "localhost")
          .withHttpApp(httpApp(chatTopic))
          .serve
          .compile
          .drain
    } yield ExitCode.Success
}

object WebSocketClient extends IOApp {
  private val uri = uri"ws://localhost:9002/game"

  private def printLine(string: String = ""): IO[Unit] = IO(println(string))

  override def run(args: List[String]): IO[ExitCode] = {
    val clientRange = Resource
      .eval(IO(HttpClient.newHttpClient()))
      .flatMap(JdkWSClient[IO](_).connectHighLevel(WSRequest(uri)))
    val minRange = 0
    val maxRange = 100000

    clientRange.use { client =>
      for {
        _ <- client.send(WSFrame.Text(s"range MyLord $minRange $maxRange"))
        _ <-
          client.receiveStream
            .collectFirst {
              case WSFrame.Text(s, _) => s
            }
            .compile
            .string >>= printLine

        _ <- tryingGues(minRange: Int, maxRange: Int)(client) >>= printLine
      } yield ExitCode.Success
    }

  }

  def tryingGues(minRange: Int, maxRange: Int)(
    client: WSConnectionHighLevel[IO]
  ): IO[String] = {
    for {
      _ <- client.send(
        WSFrame.Text(s"guess MyLord ${(maxRange - minRange) / 2 + minRange}")
      )

      recive <- client.receive
      serverResponse = recive match {
        case Some(WSFrame.Text(message, _)) => message.trim
      }
      hiddenNumber <- {
        serverResponse match {
          case `great` =>
            println(
              s"Hidden number is greater then ${(maxRange - minRange) / 2 + minRange}"
            );
            tryingGues((maxRange - minRange) / 2 + minRange, maxRange)(client)

          case `low` =>
            println(
              s"Hidden number is lower then ${(maxRange - minRange) / 2 + minRange}"
            );
            tryingGues(minRange, (maxRange - minRange) / 2 + minRange)(client)
          case _ =>
            IO.pure(
              s"\nYou guessed it, the hidden number is ${(maxRange - minRange) / 2 + minRange}"
            )
        }
      }
    } yield hiddenNumber

  }
}
