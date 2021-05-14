import DbCommon._
import cats.effect._
import doobie._
import doobie.implicits._

import java.time.Year
import java.util.UUID

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    DbTransactor
      .pooled[IO]
      .use { xa =>
        for {
          _ <- setup().transact(xa)
          _ <- WebSocketServer.run(xa)
        } yield ()
      }
      .as(ExitCode.Success)

  implicit val uuidMeta: Meta[UUID] =
    Meta[String].timap(UUID.fromString)(_.toString)
  implicit val yearMeta: Meta[Year] = Meta[Int].timap(Year.of)(_.getValue)

  // setup
  val ddl1 = Fragment.const(createTableAuthorsSql)
  val ddl2 = Fragment.const(createTableBooksSql)
  val dml = Fragment.const(populateDataSql)

  def setup(): ConnectionIO[Unit] =
    for {
      _ <- ddl1.update.run
      _ <- ddl2.update.run
      _ <- dml.update.run
    } yield ()
}
