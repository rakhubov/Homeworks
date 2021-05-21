import java.util.UUID

object GameData {

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

  final case class PlayerAtTable(
      playerID: UUID,
      tableID: UUID,
      name: String,
      playerCard: String = "", //List[Int] = List(),
      tableAndPlayerCard: String = "", //List[Int] = List()
      cardForCombination: String = "", //List[Int] = List(),
      combination: Int = 0
//      money: Int = 0,
//      playerBid: Int = 0
  )

  val cardIntToString = Map()
}
