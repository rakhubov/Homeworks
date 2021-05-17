import java.util.UUID

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
    money: Int,
    playerBid: Int = 0,
    playerCard: List[Int] = List(),
    tableAndPlayerCard: List[Int] = List(),
    cardForCombination: List[Int] = List(),
    combination: Int = 0
)
