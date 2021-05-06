import cats.effect._
import cats.syntax.all._
import cats.effect.{Bracket, Effect, ExitCode, IO, IOApp}
import cats.implicits.toSemigroupKOps
import cats.data.NonEmptyList
import cats.implicits._
import io.circe.generic.auto._
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.global
import scala.util.Random

import org.http4s._
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.dsl.io._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.{HttpApp, HttpRoutes, ParseFailure, QueryParamDecoder}
import org.http4s.circe.CirceEntityCodec._
import org.http4s.Method.{GET, POST, DELETE}
import org.http4s.dsl.io._
import org.http4s.circe.CirceEntityCodec.{circeEntityDecoder, circeEntityEncoder}



import doobie.{Fragment, Fragments, Transactor}
import doobie.h2._

import java.time.LocalDate
import java.time.Year
import java.util.UUID

import Protoco._
import DbCommon._
import Main._
import DbTransactor._
import DbConfig._


import doobie.implicits._
import doobie.implicits.javatime._
import doobie.{ConnectionIO, Fragment, Meta}
import doobie.h2._


object Protoco {
  final case class User(name: String, minRange: Int, maxRange: Int)
  final case class TryingGuess(name: String, tryingNomber: Int)
  final case class Resp(response: String)
  val randomNomberForUser = scala.collection.mutable.Map[String, Int]()
}

object ServerHttp {
 def run(xa: Transactor[IO])(implicit concurent: ConcurrentEffect[IO], timer: Timer[IO]): IO[ExitCode] =
    BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 9001, host = "localhost")
      .withHttpApp(httpRoute(xa).orNotFound)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
  // ...

  private def rangeNomberRoutes(xa: Transactor[IO]):HttpRoutes[IO] = {
    HttpRoutes.of[IO] {

      case req @ POST -> Root / "range" =>
        for {

          response <- Ok("books")
        } yield response
    }
  }

  private def tryingGuessRoutes(xa: Transactor[IO]): HttpRoutes[IO] = {
    HttpRoutes.of[IO] {

      case req @ POST -> Root / "guess" =>
        for {
          response <- Ok("books")
        } yield response
    }
  }

  private def httpRoute(xa: Transactor[IO]): HttpRoutes[IO] = {
    rangeNomberRoutes(xa) <+> tryingGuessRoutes(xa)
  }


  //          // business part
  //          _ <- fetchAuthorById(authorOdersky).option.transact(xa).map(println)
  //
  //          //_ <- fetchAuthorById(UUID.randomUUID()).option.transact(xa).map(println)
  //          //          _ <- fetchHarryPotterBooks.to[List].transact(xa).map(_.foreach(println))
  //          //          _ <- fetchBooksByAuthors(NonEmptyList.of(authorOdersky, authorRowling))
  //          //            .to[List]
  //          //            .transact(xa)
  //          //            .map(_.foreach(println))
  //          //          _ <- fetchBooksByYear(1998).transact(xa).map(_.foreach(println))
  //          //          _ <- fetchBooksByYearRange(1997, 2001).transact(xa).map(_.foreach(println))
  //          //          _ <-
  //          //            (insertBook("Harry Potter and the Cursed Child - Parts I & II", authorRowling, Year.of(2016)) *>
  //          //              fetchBooksByAuthors(NonEmptyList.of(authorRowling)).to[List])
  //          //              .transact(xa)
  //          //              .map(_.foreach(println))
  //          //_ <- updateYearOfBook(bookHPStone, Year.of(2003)).transact(xa)
  //
  val authors: Fragment =
  fr"SELECT id, name, birthday FROM authors"

  val books: Fragment =
    fr"SELECT id, author, title, year FROM books"

  def fetchAuthorById(id: UUID): doobie.Query0[Author] =
    (authors ++ fr"WHERE id = $id").query[Author]

  val fetchBooksAndAuthor: Fragment =
    fr"""SELECT b.id, a.id, a.name, a.birthday, b.title, b.year FROM books b
            INNER JOIN authors a ON b.author = a.id"""

  val fetchHarryPotterBooks: doobie.Query0[BookWithAuthor] = {
    //    val queryAllBooks = Fragment.const(
    //      """SELECT b.id, a.id, a.name, a.birthday, b.title, b.year FROM books b
    //          INNER JOIN authors a ON b.author = a.id WHERE b.author = '$authorId2';""".stripMargin,
    //    )
    val queryHPBooks = fetchBooksAndAuthor ++ fr"WHERE b.author = $authorRowling;"
    queryHPBooks.query[BookWithAuthor]
  }

  def fetchBooksByAuthors(ids: NonEmptyList[UUID]): doobie.Query0[BookWithAuthor] = {
    val queryBooks = fetchBooksAndAuthor ++ fr"WHERE" ++ Fragments.in(fr"author", ids)
    queryBooks.query[BookWithAuthor]
  }

  def fetchBooksByYear(year: Int): doobie.ConnectionIO[List[Book]] =
    (books ++ fr"WHERE year = $year").query[Book].to[List]


  def fetchBooksByYearRange(yearFrom: Int, yearTo: Int): doobie.ConnectionIO[List[Book]] = (books ++ fr"WHERE year BETWEEN $yearFrom AND $yearTo").query[Book].to[List]

  def insertBook(title: String, authorId: UUID, year: Year): doobie.ConnectionIO[Int] =
    fr"INSERT INTO books (id, author, title, year) VALUES (${UUID.randomUUID()}, $authorId, $title, $year)".update.run


  def updateYearOfBook(id: UUID, year: Year): doobie.ConnectionIO[Int] = fr"UPDATE books SET year = $year WHERE id = $id".update.run

}
