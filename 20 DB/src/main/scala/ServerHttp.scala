import cats.data.{NonEmptyList, Validated}
import cats.effect.{ExitCode, IO, _}
import cats.implicits.toSemigroupKOps
import doobie.implicits._
import doobie.implicits.legacy.localdate
import doobie.{Fragment, Fragments, Meta, Transactor}
import io.circe.generic.auto._
import org.http4s.Method.POST
import org.http4s.circe.CirceEntityCodec.{
  circeEntityDecoder,
  circeEntityEncoder
}
import org.http4s.dsl.io._
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.{HttpRoutes, ParseFailure, QueryParamDecoder}

import java.time.{LocalDate, Year}
import java.util.UUID
import scala.concurrent.ExecutionContext

object Protoco {
  final case class User(name: String, minRange: Int, maxRange: Int)
  final case class TryingGuess(name: String, tryingNomber: Int)
  final case class Resp(response: String)
  val randomNomberForUser = scala.collection.mutable.Map[String, Int]()
}
object ResponseForDataBase {

  implicit val UUIDDecoder: QueryParamDecoder[UUID] = { param =>
    Validated
      .catchNonFatal(UUID.fromString(param.value))
      .leftMap(t => ParseFailure(s"Failed", t.getMessage))
      .toValidatedNel
  }
  implicit val uuidMeta: Meta[UUID] =
    Meta[String].timap(UUID.fromString)(_.toString)
  implicit val yearMeta: Meta[Year] = Meta[Int].timap(Year.of)(_.getValue)
  implicit val localDateMeta: Meta[LocalDate] = localdate.JavaTimeLocalDateMeta

  val authors: Fragment =
    fr"SELECT id, name, birthday FROM authors"

  val books: Fragment =
    fr"SELECT id, author, title, genre, year FROM books"

  val fetchBooksAndAuthor: Fragment =
    fr"""SELECT b.id, a.id, a.name, a.birthday, b.title, b.year FROM books b
            INNER JOIN authors a ON b.author = a.id"""

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
    books.query[Book].to[List]

  def fetchBooksByAuthors(authorId: UUID): doobie.ConnectionIO[List[Book]] = {
    val queryBooks = books ++ fr"WHERE authorId = $authorId"
    queryBooks.query[Book].to[List]
  }

  def fetchBooksWithAuthorsByAuthor(
      ids: NonEmptyList[UUID]
  ): doobie.Query0[BookWithAuthor] = {
    val queryBooks =
      fetchBooksAndAuthor ++ fr"WHERE" ++ Fragments.in(fr"author", ids)
    queryBooks.query[BookWithAuthor]
  }

  def insertAuthor(
      name: String,
      birthday: LocalDate
  ): doobie.ConnectionIO[Int] = {
    fr"INSERT INTO authors (id, name, birthday) VALUES (${UUID
      .randomUUID()}, $name, $birthday)".update.run
  }

  def insertBook(
      authorId: UUID,
      title: String,
      genre: String,
      year: Year
  ): doobie.ConnectionIO[Int] =
    fr"INSERT INTO books (id, author, title, genre, year) VALUES (${UUID
      .randomUUID()}, $authorId, $title, $genre, $year)".update.run

  def updateAuthor(
      id: UUID,
      name: String,
      birthday: LocalDate
  ): doobie.ConnectionIO[Int] =
    fr"UPDATE authors SET (name = $name, birthday = $birthday)  WHERE id = $id".update.run

  def updateYearOfBook(id: UUID, year: Year): doobie.ConnectionIO[Int] =
    fr"UPDATE books SET year = $year WHERE id = $id".update.run

  val deleteAllBook: Fragment = fr"DELETE FROM books"

  def deleteBook(id: UUID): doobie.ConnectionIO[List[Book]] = {
    (deleteAllBook ++ fr"WHERE id = $id").update.run
    readAllBooks
  }

}

object ServerHttp {
  def run(
      xa: Transactor[IO]
  )(implicit concurent: ConcurrentEffect[IO], timer: Timer[IO]): IO[ExitCode] =
    BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 9001, host = "localhost")
      .withHttpApp(httpRoute(xa).orNotFound)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
  // ...

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

  private def createDataInTables(xa: Transactor[IO]): HttpRoutes[IO] = {
    HttpRoutes.of[IO] {

      case author @ POST -> Root / "creatAuthor" =>
        for {
          auth <- author.as[Author]
          author <- insertAuthor(auth.name, auth.birthday).transact(xa)
          response <- Ok(author)
        } yield response

      case book @ POST -> Root / "creatBook" =>
        for {
          bookk <- book.as[Book]
          book <-
            insertBook(bookk.authorId, bookk.title, bookk.genre, bookk.year)
              .transact(xa)
          response <- Ok(book)
        } yield response
    }
  }

  private def updateDataInTables(xa: Transactor[IO]): HttpRoutes[IO] = {
    HttpRoutes.of[IO] {

      case author @ POST -> Root / "updateAuthor" =>
        for {
          auth <- author.as[Author]
          author <- updateAuthor(auth.id, auth.name, auth.birthday).transact(xa)
          response <- Ok(author)
        } yield response

      case book @ POST -> Root / "updateBook" =>
        for {
          bookk <- book.as[Book]
          book <- updateYearOfBook(bookk.id, bookk.year)
            .transact(xa)
          response <- Ok(book)
        } yield response
    }
  }

  private def deleteData(xa: Transactor[IO]): HttpRoutes[IO] = {
    HttpRoutes.of[IO] {

      case book @ POST -> Root / "deleteBook" =>
        for {
          id <- book.as[UUID]
          book <- deleteBook(id)
            .transact(xa)
          response <- Ok(book)
        } yield response
    }
  }

  private def httpRoute(xa: Transactor[IO]): HttpRoutes[IO] = {
    readDataFromTables(xa) <+> createDataInTables(xa) <+> updateDataInTables(
      xa
    ) <+> deleteData(xa)
  }

}
