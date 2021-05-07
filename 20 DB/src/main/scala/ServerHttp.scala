import cats.effect._
import cats.implicits._
import cats.effect.{Bracket, Effect, ExitCode, IO, IOApp}
import cats.implicits.toSemigroupKOps
import cats.data.{NonEmptyList, Validated}
import io.circe.generic.auto._

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.global
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.{HttpApp, HttpRoutes, ParseFailure, QueryParamDecoder}
import org.http4s.Method.{DELETE, GET, POST}
import org.http4s.dsl.io._
import org.http4s.circe.CirceEntityCodec.{circeEntityDecoder, circeEntityEncoder}
import org.http4s._
import org.http4s.circe.CirceEntityCodec._
import org.http4s.implicits._
import org.http4s.server.blaze._

import java.time.LocalDate
import java.time.Year
import java.util.UUID
import DbCommon._
import Main._
import doobie.implicits._
import doobie.implicits.legacy.localdate
import doobie.{ConnectionIO, Fragment, Meta}
import doobie.{Fragment, Fragments, Transactor}
import doobie.h2._
import doobie.Transactor


object Protoco {
  final case class User(name: String, minRange: Int, maxRange: Int)
  final case class TryingGuess(name: String, tryingNomber: Int)
  final case class Resp(response: String)
  val randomNomberForUser = scala.collection.mutable.Map[String, Int]()
}
object ResponseForDataBase{


  implicit val UUIDDecoder: QueryParamDecoder[UUID] = { param =>
    Validated
      .catchNonFatal(UUID.fromString(param.value))
      .leftMap(t => ParseFailure(s"Failed", t.getMessage))
      .toValidatedNel
  }
  implicit val uuidMeta: Meta[UUID] = Meta[String].timap(UUID.fromString)(_.toString)
  implicit val yearMeta: Meta[Year] = Meta[Int].timap(Year.of)(_.getValue)
  implicit val localDateMeta: Meta[LocalDate] = localdate.JavaTimeLocalDateMeta



  val authors: Fragment =
    fr"SELECT id, name, birthday FROM authors"

  val books: Fragment =
    fr"SELECT id, author, title, genre, year FROM books"

  def fetchAuthorById(id: UUID): doobie.Query0[Author] =
    (authors ++ fr"WHERE id = $id").query[Author]

  def fetchBookById(id: UUID): doobie.Query0[Book] =
    (books ++ fr"WHERE id = $id").query[Book]

  def fetchBookByYear(year: Int): doobie.ConnectionIO[List[Book]] =
    (books ++ fr"WHERE year = $year").query[Book].to[List]

  def readAllAuthors: doobie.ConnectionIO[List[Author]] =
    authors.query[Author].to[List]

  def fetchAuthorByName(name: String): doobie.ConnectionIO[List[Author]] =
    (authors ++ fr"WHERE name = $name").query[Author].to[List]

  def readAllBooks: doobie.ConnectionIO[List[Book]] =
    authors.query[Book].to[List]

  def fetchBooksByAuthors(ids: NonEmptyList[UUID]): doobie.Query0[BookWithAuthor] = {
    val queryBooks = fetchBooksAndAuthor ++ fr"WHERE" ++ Fragments.in(fr"author", ids)
    queryBooks.query[BookWithAuthor]
  }

  def insertAuthor(name: String, birthday: LocalDate): doobie.ConnectionIO[Int] =
    fr"INSERT INTO authors (id, name, birthday) VALUES (${UUID.randomUUID()}, $name, $birthday)".update.run

  def insertBook(title: String, authorId: UUID, genre: String, year: Year): doobie.ConnectionIO[Int] =
    fr"INSERT INTO books (id, author, title, genre, year) VALUES (${UUID.randomUUID()}, $authorId, $title, $genre, $year)".update.run

  //
  //
  //
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


  def fetchBooksByYearRange(yearFrom: Int, yearTo: Int): doobie.ConnectionIO[List[Book]] = (books ++ fr"WHERE year BETWEEN $yearFrom AND $yearTo").query[Book].to[List]


  def updateYearOfBook(id: UUID, year: Year): doobie.ConnectionIO[Int] = fr"UPDATE books SET year = $year WHERE id = $id".update.run
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

  import ResponseForDataBase._

  private def readDataFromTables(xa: Transactor[IO]):HttpRoutes[IO] = {
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

      case AuthorId @ POST -> Root / "readBookByAuthors" =>
        for {
          author <- AuthorId.as[UUID]
          book <- fetchBooksByAuthors(NonEmptyList.of(author)).to[List].transact(xa)
          response <- Ok(book)
        } yield response
    }
  }

  private def createDataInTables(xa: Transactor[IO]):HttpRoutes[IO] = {
    HttpRoutes.of[IO] {

      case author@POST -> Root / "creatAuthor" => {
        val autho = author.as[Author]
        for {
          _ <-
            insertAuthor(s"${autho.name}", authorRowling, Year.of(2016))


        } yield ()

      }

    }
  }

  private def httpRoute(xa: Transactor[IO]): HttpRoutes[IO] = {
    readDataFromTables(xa) <+> createDataInTables(xa)
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





}
