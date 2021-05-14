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
