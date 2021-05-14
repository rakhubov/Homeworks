import GameData.playerRegistration
import cats.effect.IO
import cats.effect.concurrent.Ref

import java.util.UUID

object ServerSharedCommand {

  def registrationForPlayer(message: List[String]): String = {
    //   message match {
    //     case name :: money :: Nil => playerRegistration
    //   }
    ""
  }

  def checkRequestion(message: String): String = {
    message.split("\\s+").toList match {
      case "registration" :: next => registrationForPlayer(next)

    }
  }
}
