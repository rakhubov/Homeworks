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
      xa: Transactor[IO]
  )(implicit concurent: ConcurrentEffect[IO], timer: Timer[IO]): IO[ExitCode] =
    for {
      chatTopic <- Topic[IO, String]("Hello!")
      _ <-
        BlazeServerBuilder[IO](ExecutionContext.global)
          .bindHttp(port = 8080, host = "localhost")
          .withHttpApp(httpRoute(xa, chatTopic, concurent, timer))
          .serve
          .compile
          .drain
    } yield ExitCode.Success

  private def httpRoute(
      xa: Transactor[IO],
      chatTopic: Topic[IO, String],
      concurent: ConcurrentEffect[IO],
      timer: Timer[IO]
  ) = {
    privateRoute(xa)(concurent, timer) <+> sharedRoute(
      xa,
      chatTopic
    ) <+> readDataFromTables(xa)
  }.orNotFound
  // ...
  //
  //

  import ResponseForDataBase._

  private def readDataFromTables(xa: Transactor[IO]): HttpRoutes[IO] = {
    HttpRoutes.of[IO] {

      case authorId @ POST -> Root / "readAuthorById" =>
        for {
          id <- authorId.as[UUID]
          author <- fetchAuthorById(id).option.transact(xa)
          response <- Ok(author)
        } yield response

      case POST -> Root / "readAllAuthors" =>
        for {
          authors <- readAllAuthors.transact(xa)
          response <- Ok(authors)
        } yield response

      case authorName @ POST -> Root / "readAuthorByName" =>
        for {
          name <- authorName.as[String]
          author <- fetchAuthorByName(name).transact(xa)
          response <- Ok(author)
        } yield response

      case bookId @ POST -> Root / "readBookById" =>
        for {
          id <- bookId.as[UUID]
          book <- fetchBookById(id).option.transact(xa)
          response <- Ok(book)
        } yield response

      case POST -> Root / "readAllBooks" =>
        for {
          books <- readAllBooks.transact(xa)
          response <- Ok(books)
        } yield response

      case bookYear @ POST -> Root / "readBookByYear" =>
        for {
          year <- bookYear.as[Int]
          book <- fetchBookByYear(year).transact(xa)
          response <- Ok(book)
        } yield response

      case authorId @ POST -> Root / "readBookByAuthors" =>
        for {
          author <- authorId.as[UUID]
          book <- fetchBooksByAuthors(author).transact(xa)
          response <- Ok(book)
        } yield response

      case authorId @ POST -> Root / "readBookWithAuthorByAuthors" =>
        for {
          author <- authorId.as[UUID]
          book <-
            fetchBooksWithAuthorsByAuthor(NonEmptyList.of(author))
              .to[List]
              .transact(xa)
          response <- Ok(book)
        } yield response
    }
  }

//
  //
  //

  private def privateRoute(
      xa: Transactor[IO]
  )(implicit concurent: ConcurrentEffect[IO], timer: Timer[IO]) =
    HttpRoutes.of[IO] {

      case GET -> Root / "private" =>
        val echoPipe: Pipe[IO, WebSocketFrame, WebSocketFrame] =
          _.collect {
            case WebSocketFrame.Text(message, _) =>
              message.split("\\s+").toList match {
                case "range" :: Nil =>
                  WebSocketFrame.Text("Hello33")

                case _ => WebSocketFrame.Text(s"$message")
              }
          }

        val s = Stream
          .awakeEvery[IO](1.second)
          .map(d => WebSocketFrame.Text("You are connected for " + d.toSeconds))

        for {
          queue <- Queue.bounded[IO, WebSocketFrame](20)
          response <- WebSocketBuilder[IO].build(
            receive = queue.enqueue.compose[Stream[IO, WebSocketFrame]](str =>
              str.merge(s)
            ),
            send = queue.dequeue.through(echoPipe).merge(s)
          )
        } yield response

    }
  //
  //
  //

  private def sharedRoute(
      xa: Transactor[IO],
      chatTopic: Topic[IO, String]
  ): HttpRoutes[IO] = {
    HttpRoutes.of[IO] {

      case GET -> Root / "chat" =>
        WebSocketBuilder[IO].build(
          receive =
            chatTopic.publish.compose[Stream[IO, WebSocketFrame]](_.collect {
              case WebSocketFrame.Text(message, _) => message.trim
            }.pull.uncons1.flatMap {
              case Some((name, tail)) =>
                tail.map(message => s"$name: $message").pull.echo
              case None => Pull.done
            }.stream),
          send = chatTopic.subscribe(20).map(WebSocketFrame.Text(_))
        )

      case author @ POST -> Root / "creatAuthor" =>
        for {
          auth <- author.as[Author]
          author <- insertAuthor(auth.name, auth.birthday).transact(xa)
          response <- Ok(author)
        } yield response

    }
  }

}
