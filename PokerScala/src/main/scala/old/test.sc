import GameData._

import java.util.UUID
import cats.effect.IO
import io.chrisdavenport.fuuid.FUUID


val s =  List(1,2,3,4,5,6)

List().headOption.getOrElse("")

val uuid1 = FUUID.fromString("7cfb70a9-0764-4851-a28c-309393aea2eb")
val g: UUID = uuid1 match {
  case Right(value) => UUID.fromString(value.toString)
  case _ => UUID.randomUUID()
}
if(s.toVector.lift(1) == Option(1)) 5
s.sorted.reverse
s.forall(_ < 10)