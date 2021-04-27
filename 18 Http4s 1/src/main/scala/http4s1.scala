import cats.effect._
import cats.syntax.all._
import io.circe.generic.auto._
import org.http4s.circe.CirceEntityCodec._

import Protoco._
import org.http4s._
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.dsl.io._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder

import scala.concurrent.ExecutionContext
import scala.util.Random


// Homework. Place the solution under `http` package in your homework repository.
//
// Write a server and a client that play a number guessing game together.
//
// Communication flow should be as follows:
// 1. The client asks the server to start a new game by providing the minimum and the maximum number that can
// be guessed.
// 2. The server comes up with some random number within the provided range.
// 3. The client starts guessing the number. Upon each attempt, the server evaluates the guess and responds to
// the client, whether the current number is lower, greater or equal to the guessed one.
// 4. The game ends when the number is guessed or there are no more attempts left. At this point the client
// should terminate, while the server may continue running forever.
// 5. The server should support playing many separate games (with different clients) at the same time.
//
// The exact protocol and message format to use is not specified and should be designed while working on the task.
object Protoco {
  final case class User(name: String, minRange: Int, maxRange: Int)
  final case class TryingGuess(name: String, tryingNomber: Int)
  final case class Resp(response: String)
  val randomNomberForUser = scala.collection.mutable.Map[String, Int]()
}



object GuessServer extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 9001, host = "localhost")
      .withHttpApp(httpApp)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
  // ...

  private val rangeNomberRoutes = {
    HttpRoutes.of[IO] {

      case req @ POST -> Root / "range" =>
        req.as[User].flatMap { user =>
          randomNomberForUser += (user.name -> Random
            .between(user.minRange, user.maxRange))
          Ok(
            s"Hello, ${user.name}! The server guessed a number in the range from ${user.minRange} to ${user.maxRange}. Can you guess?"
          )
        }
    }
  }

  private val tryingGuessRoutes = {
    HttpRoutes.of[IO] {

      case req @ POST -> Root / "guess" =>
        req.as[TryingGuess].flatMap { trying =>
          if (
            (randomNomberForUser get trying.name) > Option(trying.tryingNomber)
          )
            Ok(Resp("Great"))
          else if (
            (randomNomberForUser get trying.name) < Option(trying.tryingNomber)
          )
            Ok(Resp("Low"))
          else Ok(Resp("Equal"))
        }
    }
  }

  private val httpApp = {
    rangeNomberRoutes <+> tryingGuessRoutes
  }.orNotFound
}
object GuessClient extends IOApp {

  private val uri = uri"http://localhost:9001"

  private def printLine(string: String = ""): IO[Unit] = IO(println(string))


  def run(args: List[String]): IO[ExitCode] =
    BlazeClientBuilder[IO](ExecutionContext.global).resource
      .use {
        case client =>
          val minRange = 0
          val maxRange = 10000000
          for {
            _ <- printLine(string = "Start Gaming")
            _ <-
              client
                .expect[String](
                  Method.POST(User("John", minRange, maxRange), uri / "range")) >>= printLine
            _ <- printLine()

            _ <- tryingGuess(minRange: Int, maxRange: Int)(client) >>= printLine
            _ <- printLine()
          } yield ()
      }
      .as(ExitCode.Success)

  def tryingGuess(minRange: Int, maxRange: Int)(
    client: Client[IO]
  ): IO[String] = {
    client
      .expect[Resp](
        Method
          .POST(TryingGuess("John", (maxRange - minRange) / 2 + minRange), uri / "guess"))
      .flatMap{resp => {if (resp.response == "Great")  {println(s"Hidden number is greater then ${(maxRange - minRange) / 2 + minRange}"); tryingGuess((maxRange - minRange) / 2 + minRange, maxRange)(client)}
      else if (resp.response == "Low") { println(s"Hidden number is lower then ${(maxRange - minRange) / 2 + minRange}"); tryingGuess(minRange, (maxRange - minRange) / 2 + minRange)(client)}
      else  {println(); IO.pure(s"You guessed it, the hidden number is ${(maxRange - minRange) / 2 + minRange}") }}}
  }
}