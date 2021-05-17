import cats.effect._
import doobie._
import doobie.implicits._
import java.util.UUID

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    DbTransactor
      .pooled[IO]
      .use { connectToDataBase =>
        for {
          _ <- CreateDB.setup().transact(connectToDataBase)
          _ <- WebSocketServer.run(connectToDataBase)
        } yield ()
      }
      .as(ExitCode.Success)

}
