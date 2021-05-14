import cats.effect.IO
import cats.effect.concurrent.Ref

import java.util.UUID

object GameData {

  lazy val playerRegistration = Ref[IO].of(Map(UUID.randomUUID() -> ""))

  final case class Player(
      name: String,
      money: Int,
      playerBid: Int = 0,
      playerCard: List[Int] = List(),
      tableAndPlayerCard: List[Int] = List(),
      cardForCombination: List[Int] = List(),
      combination: Int = 0
  )

}
