import GameData.{Player, logger}
import cats.effect.IO
import cats.effect.concurrent.Ref

object CheckCombination {
  def streetFlush(playerRef: Ref[IO, Player]): IO[Unit] = {
    playerRef
      .modify(player => ((player), player))
      .flatMap(i => logger.info(i.toString))
  }

}
