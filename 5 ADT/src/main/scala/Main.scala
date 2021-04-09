object Main {

  sealed trait Gamer

  final case object NameGame extends Gamer { var game: Int = 0 }
  final case object Rav extends Gamer { var game: Int = 0 }
  final case object CardinGame extends { val cardinGame = Array[Int] }
  final case object CardofGamer extends { val cardofGamer = Array[Int] }
  final case object PowerCombination extends { var powerCombination = 0 }
  final case object PowerCard extends { val powerCard = Array[Int] }
}

sealed trait SymbolString
object SymbolString {

  final case object Club extends SymbolString

  final case object Diamond extends SymbolString

  final case object Heart extends SymbolString

  final case object Spade extends SymbolString

  final case object Two extends SymbolString

  final case object Three extends SymbolString

  final case object Four extends SymbolString

  final case object Five extends SymbolString

  final case object Six extends SymbolString

  final case object Seven extends SymbolString

  final case object Eight extends SymbolString

  final case object Nine extends SymbolString

  final case object Ten extends SymbolString

  final case object Jack extends SymbolString

  final case object Queen extends SymbolString

  final case object King extends SymbolString

  final case object Ace extends SymbolString

  def rename(value: String): Either[String, SymbolString] = {
    value match {

      case "c" => Right(Club)
      case "d" => Right(Diamond)
      case "h" => Right(Heart)
      case "s" => Right(Spade)
      case "2" => Right(Two)
      case "3" => Right(Three)
      case "4" => Right(Four)
      case "5" => Right(Five)
      case "6" => Right(Six)
      case "7" => Right(Seven)
      case "8" => Right(Eight)
      case "9" => Right(Nine)
      case "T" => Right(Ten)
      case "J" => Right(Jack)
      case "Q" => Right(Queen)
      case "K" => Right(King)
      case "A" => Right(Ace)
      case _   => Left("Incorrect Symbol")
    }
  }

  sealed trait NameGame

  object NameGame {

    final case object TexasHoldem extends NameGame

    final case object OmahaHoldem extends NameGame

    final case object FifeCardDraaw extends NameGame

    def game(value: String): Either[String, NameGame] = {
      value match {

        case "texas-xoldem"   => Right(TexasHoldem)
        case "omaha-holdem"   => Right(OmahaHoldem)
        case "five-card-draw" => Right(FifeCardDraaw)
        case _                => Left("Incorrect name game")
      }
    }
  }
  sealed trait PowerCombination
  object PokerCombination {
    final object RoyalFlush extends PowerCombination
    final object StraightFlush extends PowerCombination
    final object FourOfAKind extends PowerCombination
    final object FullHouse extends PowerCombination
    final object Flush extends PowerCombination
    final object Straight extends PowerCombination
    final object ThreeOfAKind extends PowerCombination
    final object TwoPair extends PowerCombination
    final object Pair extends PowerCombination
    final object HighCard extends PowerCombination

    def powerCombination(
        value: Array[Int]
    ): Either[String, PowerCombination] = {
      value match {

        case 1 => Right(HighCard)
        case 2 => Right(Pair)
        case 3 => Right(TwoPair)
        case 4 => Right(ThreeOfAKind)
        case 5 => Right(Straight)
        case 6 => Right(Flush)
        case 7 => Right(FullHouse)
        case 8 => Right(FourOfAKind)
        case 9 => Right(StraightFlush)
        case _ => Left("Incorrect power card")
      }
    }
  }
}
