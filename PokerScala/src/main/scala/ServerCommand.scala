import cats.effect.IO
import cats.effect.concurrent.Ref
import doobie.Transactor
import doobie.implicits._

import java.util.UUID
import RequestInDB._

import scala.util.control.Breaks.break

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
            //           registrationInDB(id, name, moneyInt).transact(connectToDataBase);
            (for {
              _ <-
                registrationInDB(id, "Lord", 1000).transact(connectToDataBase)
              dfd <-
                fetchMoneyPlayerAccountByID(id)
                  .to[List]
                  .transact(connectToDataBase)
                  .map(_.foreach(println))
              _ = println(dfd)
              _ = println("---000---")
              str = s"id $id"
            } yield str).unsafeRunSync()
          //       s"id $id"
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
      case _ => s"$message is invalid private request"
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
            (for {
              moneyPlayerAccount <-
//                fetchMoneyPlayerAccountByID(validPlayerID).option
//                  .transact(
//                    connectToDataBase
//                  )
                fetchMoneyPlayerAccountAll
                  .to[List]
                  .transact((connectToDataBase))
              _ = println(moneyPlayerAccount)
              tablesID <- moneyPlayerAccount.headOption match {
                case Some(money) =>
                  if (money >= validMoney && validMoney >= validBid * 10) {
                    //funk (accaunt - moneytable)
                    fetchTableByBidNotStart(validBid)
                      .to[List]
                      .transact(
                        connectToDataBase
                      )
                  } else break
                case _ => break
              }
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
              //all player
              messageComplite =
                s"PlayerAtTable $nameString $tableID $validMoney"
            } yield messageComplite).unsafeRunSync()
        }
      case _ =>
        s"error $message is invalid data, money need be more then bid * 10"
    }
  }

  def checkSharedRequestion(
      message: String,
      connectToDataBase: Transactor[IO]
  ): String = {
    message.split("\\s+").toList match {
      case "game" :: next =>
        tableSearch(next, connectToDataBase)
      case _ => s"$message is invalid shared request"
    }
  }
}
