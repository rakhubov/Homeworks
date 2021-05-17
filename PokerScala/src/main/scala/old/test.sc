import cats.effect._
import cats.effect.concurrent.Ref
import doobie.Meta
import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.{catsSyntaxFlatMapOps, catsSyntaxPartialOrder}

import scala.concurrent.duration.DurationInt
import java.util.UUID
import scala.concurrent.Future.never.value

implicit val uuidMeta: String =>
  UUID = {x => UUID.fromString(x)}

val bookHPStone: UUID = UUID.randomUUID()
val v =bookHPStone.toString
val b:UUID = UUID.fromString(v)
bookHPStone == bookHPStone
bookHPStone == v

