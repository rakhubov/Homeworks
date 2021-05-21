import GameData._
import RefactorFunction._
import RequestInDB.{playerSitsAtTable, writeAllPlayerCard}
import cats.effect.IO
import cats.effect.concurrent.Ref
import doobie.Transactor
import io.chrisdavenport.fuuid.FUUID
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger

import java.util.UUID

object SearchWinner {

  def searchWinner(
      listPlayers: List[PlayerDB],
      connectToDataBase: Transactor[IO]
  ): IO[Unit] =
    if (listPlayers.size > 0) {
      val player = PlayerFromPlayerDB(
        listPlayers.headOption.getOrElse(PlayerDB())
      )
      if (
        (player.playerCard ++ player.tableAndPlayerCard).size == 9 &&
        ((player.playerCard ++ player.tableAndPlayerCard)
          .contains(53) == false)
      ) {
        logger.info(s"consumed ")
        val playerRef: IO[Ref[IO, Player]] = Ref.of[IO, Player](Player())

        for {

//        _ <- writeAllPlayerCard(stringTablePlayerCard, twoCard, playerID)
//          .transact(connectToDataBase)
          _ <- searchWinner(listPlayers.tail, connectToDataBase)
        } yield ()
      } else IO.unit
    } else IO.unit
}
