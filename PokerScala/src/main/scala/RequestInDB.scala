import cats.data.{NonEmptyList, Validated}
import cats.effect.{ExitCode, IO, _}
import cats.implicits.toSemigroupKOps
import doobie.implicits._
import doobie.implicits.legacy.localdate
import doobie.{Fragment, Fragments, Meta, Transactor}
import io.circe.generic.auto._
import org.http4s.Method.POST
import org.http4s.circe.CirceEntityCodec.{
  circeEntityDecoder,
  circeEntityEncoder
}
import org.http4s.dsl.io._
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.{HttpRoutes, ParseFailure, QueryParamDecoder}

import java.time.{LocalDate, Year}
import java.util.UUID
import scala.concurrent.ExecutionContext
import doobie.implicits._
import doobie.implicits.legacy.localdate
import doobie.{Fragment, Fragments, Meta, Transactor}

import java.util.UUID

object RequestInDB {

  implicit val uuidMeta: Meta[UUID] =
    Meta[String].timap(UUID.fromString)(_.toString)

  val tables: Fragment =
    fr"SELECT id, startGame, idPlayer," ++
      fr" bidForTable, dealerName, playerInGame," ++
      fr" numberOpenCard, generatedCards FROM tables"

  val tablesID: Fragment =
    fr"SELECT id FROM tables"

  val registrationMoney: Fragment =
    fr"SELECT moneyPersonalAccount FROM registration"

  val playerName: Fragment =
    fr"SELECT name FROM registration"

  val addRegistration: Fragment = fr"INSERT INTO registration" ++
    fr" (id, name, moneyPersonalAccount) VALUES"

  val addTable: Fragment = fr"INSERT INTO tables" ++
    fr" (id, startGame, bidForTable, numberOpenCard) VALUES"

  val addPlayer: Fragment = fr"INSERT INTO players" ++
    fr" (playerID, tableID, name, money) VALUES"

  def registrationInDB(
      id: UUID,
      name: String,
      moneyPersonalAccount: Int
  ): doobie.ConnectionIO[Int] =
    (addRegistration ++ fr" ($id, $name, $moneyPersonalAccount)").update.run

  def fetchTableByBidNotStart(bid: Int): doobie.Query0[UUID] =
    (tablesID ++ fr"WHERE bidForTable = $bid AND startGame = 0").query[UUID]

  def createTable(id: UUID, validBid: Int): doobie.ConnectionIO[Int] =
    (addTable ++ fr" ($id, 0, $validBid, 0)").update.run

  def fetchNameByID(validPlayerID: UUID): doobie.Query0[String] =
    (playerName ++ fr"WHERE id = $validPlayerID").query[String]

  def createPlayer(
      validPlayerID: UUID,
      validMoney: Int,
      tableID: UUID,
      name: String
  ): doobie.ConnectionIO[Int] =
    (addPlayer ++ fr" ($validPlayerID, $tableID, $name, $validMoney)").update.run

  def playerSitsAtTable(
      validPlayerID: String,
      tableID: UUID
  ): doobie.ConnectionIO[Int] =
    (fr"UPDATE tables SET (idPlayer = CASE WHEN" ++
      fr" idPlayer IS NULL THEN $validPlayerID ELSE" ++
      fr" CONCAT(idPlayer, ' ', $validPlayerID)) END" ++
      fr" WHERE id = $tableID").update.run

  def fetchMoneyPlayerAccountByID(playerId: UUID): doobie.Query0[Int] =
    (registrationMoney ++ fr"WHERE id = $playerId").query[Int]
}
