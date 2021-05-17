package old

import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.catsSyntaxFlatMapOps

import scala.concurrent.duration.DurationInt

object test2 extends IOApp {

  def periodicReader(ref: Ref[IO, Int]): IO[Unit] =
    IO.sleep(1.second) >> ref.get.flatMap(i =>
      IO(println(s"Current value is $i"))
    ) >> periodicReader(ref)

  def periodicIncrementer(ref: Ref[IO, Int]): IO[Unit] =
    IO.sleep(750.millis) >> ref.update(_ + 1) >> periodicIncrementer(ref)

  override def run(args: List[String]): IO[ExitCode] = {
    val ref = Ref[IO].of(0)

    for {
      _ <- ref.flatMap(_.update(_ + 42))
      x <- ref.flatMap(_.get)
      _ <- IO(println(s"The value is $x"))
    } yield ExitCode.Success
  }
}
