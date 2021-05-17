import cats.effect.IO
import cats.effect.concurrent.Ref
import doobie.Transactor
import doobie.implicits._
import java.util.UUID
import RequestInDB._

object ServerPrivateCommand {

  def registrationForPlayer(
      message: List[String],
      connectToDataBase: Transactor[IO]
  ): String = {
    message match {
      case name :: money :: Nil =>
        money.toIntOption match {
          case Some(moneyInt) =>
            val id = UUID.randomUUID();
            registrationInDB(id, name, moneyInt).transact(connectToDataBase);
            s"you id $id"
        }
      case _ => s"$message is invalid data"
    }
  }

  def checkPrivatRequestion(
      message: String,
      connectToDataBase: Transactor[IO]
  ): String = {
    message.split("\\s+").toList match {
      case "registration" :: next =>
        registrationForPlayer(next, connectToDataBase)
      case _ => s"$message is invalid request"
    }
  }
}

//
//
//
object ServerSharedCommand {

  def tableSearch(
      message: List[String],
      connectToDataBase: Transactor[IO]
  ): String = {
    message match {
      case playerID :: bid :: money :: Nil =>
        (
          Option(UUID.fromString(playerID)),
          bid.toIntOption,
          money.toIntOption
        ) match {
          case (Some(validPlayerID), Some(validBid), Some(validMoney)) =>
            for {
              moneyPlayerAccount <-
                fetchMoneyPlayerAccountByID(validPlayerID)
                  .option
                  .transact(
                    connectToDataBase
                  )
              moneySome = moneyPlayerAccount match {
                case Some(value) => value}
            moneyMessage = if(moneySome > validBid * 10) ""

            } yield ()
        }

      case playerID :: bid :: money :: Nil =>
        (
          Option(UUID.fromString(playerID)),
          bid.toIntOption,
          money.toIntOption
        ) match {
          case (Some(validPlayerID), Some(validBid), Some(validMoney)) =>
            for {
              tablesID <-
                fetchTableByBidNotStart(validBid)
                  .to[List]
                  .transact(
                    connectToDataBase
                  )
              tableID = tablesID.headOption match {
                case Some(id) => id
                case _ =>
                  val id = UUID.randomUUID();
                  createTable(id, validBid).transact(connectToDataBase); id
              }
              name <-
                fetchNameByID(validPlayerID).option
                  .transact(connectToDataBase)
              nameString = name match {
                case Some(value) => value
              }
              _ <- createPlayer(
                validPlayerID,
                validMoney,
                tableID,
                nameString
              ).transact(connectToDataBase)
              _ <- playerSitsAtTable(playerID, tableID).transact(
                connectToDataBase
              )
            } yield () s"Player $playerID sat down at the table with a bet of $bid"
        }
      case _ => s"$message is invalid data"
    }
  }

  def checkSharedRequestion(
      message: String,
      connectToDataBase: Transactor[IO]
  ): String = {
    message.split("\\s+").toList match {
      case "game" :: next =>
        tableSearch(next, connectToDataBase)
      case _ => s"$message is invalid request"
    }
  }
}
