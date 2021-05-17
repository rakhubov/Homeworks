import cats.effect._
import cats.effect.concurrent.Ref
import doobie.Meta
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.{catsSyntaxFlatMapOps, catsSyntaxPartialOrder}

import scala.concurrent.duration.DurationInt
import java.util.UUID
import scala.concurrent.Future.never.value


val id = UUID.randomUUID()
s"dsdsd $id"
