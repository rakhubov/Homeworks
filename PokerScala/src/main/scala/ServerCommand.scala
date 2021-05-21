import CardManipulation._
import cats.effect.IO
import cats.effect.concurrent.Ref
import doobie.Transactor
import doobie.implicits._
import GameData._

import java.util.UUID
import RequestInDB._
import cats.implicits.catsSyntaxApplicativeId
import org.http4s.client.jdkhttpclient.WSFrame.Text
object ServerPrivateCommand {

  def registrationForPlayer(
      message: List[String],
      connectToDataBase: Transactor[IO]
  ): IO[String] = {
    message match {
      case name :: money :: Nil =>
        money.toIntOption match {
          case Some(moneyInt) =>
            val id = UUID.randomUUID();
            for {
              _ <-
                registrationInDB(id, name, moneyInt).transact(connectToDataBase)
              response = s"id $id"
            } yield response
          case _ => IO(s"error $message money invalid format")
        }
      case _ => IO(s"error $message data not parsing")
    }
  }

  def checkPrivatRequestion(
      message: String,
      connectToDataBase: Transactor[IO]
  ): IO[String] = {
    message.split("\\s+").toList match {
      case "registration" :: next =>
        registrationForPlayer(next, connectToDataBase)
      case _ => IO(s"error $message invalid private request")
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
  ): IO[String] = {
    message match {
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
              refTableID <- Ref.of[IO, UUID](UUID.randomUUID())
              _ <- tablesID.headOption match {
                case Some(id) => {
                  refTableID.set(id).void
                }
                case _ => {
                  val id = UUID.randomUUID();
                  refTableID.set(id) *>
                    createTable(id, validBid).transact(connectToDataBase)
                }
              }
              tableID <- refTableID.get
              name <-
                fetchNameByID(validPlayerID).option
                  .transact(connectToDataBase)
              nameString = name match {
                case Some(value) => value
                case _           => "unknown"
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
            } yield messageComplite
          case _ => IO(s"error $message data not parsing")
        }
    }
  }

  def startGame(
      playerID: String,
      connectToDataBase: Transactor[IO]
  ): IO[String] =
    for {
      tableID <-
        fetchTableByPlayerID(UUID.fromString(playerID)).option
          .transact(
            connectToDataBase
          )
      someTableID = tableID.getOrElse(UUID.randomUUID())
      _ <- startGameForTable(someTableID)
        .transact(connectToDataBase)
      listPlayersID <-
        fetchListPlayerID(someTableID).option
          .transact(
            connectToDataBase
          )
      stringListID = listPlayersID.getOrElse("").trim
      playersNumber = stringListID.split("\\s+").toList.size
      numberCard = 5 + playersNumber * 2
      allCardInGame = generationCard(numberCard).toList
      cardTable = allCardInGame.take(5)
      allCardInHands = allCardInGame.takeRight(numberCard - 5)
      _ <- writePlayerCard(
        cardTable,
        allCardInHands,
        stringListID.split("\\s+").toList,
        connectToDataBase
      )
      player <- fetchPlayers.transact(connectToDataBase)
      response = s"$player" //= s"start ${tableID.getOrElse(UUID.randomUUID())}"
    } yield response

  def checkSharedRequestion(
      message: String,
      connectToDataBase: Transactor[IO]
  ): IO[String] = {
    message.split("\\s+").toList match {
      case "game" :: next =>
        tableSearch(next, connectToDataBase)
      case "start" :: id :: Nil =>
        startGame(id, connectToDataBase)
      case _ => IO("invalid request")

    }
  }
}
