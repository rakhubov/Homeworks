import cats.effect._
import cats.effect.concurrent.Ref

val player = Ref.of[IO, Map[String, Int]](Map("1" -> 2))

val players = Ref[IO].of(List(12))
players.map(_.update(_ => List(23))).unsafeRunSync()

val as =
 for {
  ref   <- players
  value <- ref.get

 v2 = value.map(_ *2)
} yield v2
val z =
 as.unsafeRunSync()
z
