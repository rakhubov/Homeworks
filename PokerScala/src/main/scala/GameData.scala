import cats.effect.IO

import java.util.UUID
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger

object GameData {

  val logger = Slf4jLogger.getLogger[IO]

  final case class PlayerRegistration(
      id: UUID,
      name: String,
      moneyPersonalAccount: Int
  )

  final case class GameTable(
      id: UUID,
      startGame: Boolean = false,
      idPlayer: List[UUID] = List(),
      bidForTable: Int = 0,
      dealerName: UUID = UUID.randomUUID(),
      playerInGame: List[String] = List(),
      numberOpenCard: Int = 0,
      generatedCards: Set[Int] = Set()
  )

  final case class PlayerDB(
      playerID: UUID = UUID.randomUUID(),
      tableID: UUID = UUID.randomUUID(),
      name: String = "",
      playerCard: String = "", //List[Int] = List(),
      tableAndPlayerCard: String = "", //List[Int] = List()
      cardForCombination: String = "", //List[Int] = List(),
      combination: Int = 0
//      money: Int = 0,
//      playerBid: Int = 0
  )

  final case class Player(
      playerID: UUID = UUID.randomUUID(),
      name: String = "",
      playerCard: List[Int] = List(),
      allCard: Vector[Int] = Vector(),
      cardForCombination: List[Int] = List(),
      combination: Int = 0
  )

  val cardIntToString =
    Map[Int, String](
      51 -> "Ace of spades",
      50 -> "King of spades",
      49 -> "Queen of spades",
      48 -> "jack of spades",
      47 -> "10 of spades",
      46 -> "9 of spades",
      45 -> "8 of spades",
      44 -> "7 of spades",
      43 -> "6 of spades",
      42 -> "5 of spades",
      41 -> "4 of spades",
      40 -> "3 of spades",
      39 -> "2 of spades",
      38 -> "Ace of hearts",
      37 -> "King of hearts",
      36 -> "Queen of hearts",
      35 -> "jack of hearts",
      34 -> "10 of hearts",
      33 -> "9 of hearts",
      32 -> "8 of hearts",
      31 -> "7 of hearts",
      30 -> "6 of hearts",
      29 -> "5 of hearts",
      28 -> "4 of hearts",
      27 -> "3 of hearts",
      26 -> "2 of hearts",
      25 -> "Ace of diamonds",
      24 -> "King of diamonds",
      23 -> "Queen of diamonds",
      22 -> "jack of diamonds",
      21 -> "10 of diamonds",
      20 -> "9 of diamonds",
      19 -> "8 of diamonds",
      18 -> "7 of diamonds",
      17 -> "6 of diamonds",
      16 -> "5 of diamonds",
      15 -> "4 of diamonds",
      14 -> "3 of diamonds",
      13 -> "2 of diamonds",
      12 -> "Ace of clubs",
      11 -> "King of clubs",
      10 -> "Queen of clubs",
      9 -> "jack of clubs",
      8 -> "10 of clubs",
      7 -> "9 of clubs",
      6 -> "8 of clubs",
      5 -> "7 of clubs",
      4 -> "6 of clubs",
      3 -> "5 of clubs",
      2 -> "4 of clubs",
      1 -> "3 of clubs",
      0 -> "2 of clubs"
    )
}
