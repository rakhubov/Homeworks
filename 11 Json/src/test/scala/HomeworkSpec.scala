import HomeworkSpec.TeamTotals

import java.time.{LocalDate, ZonedDateTime}
import cats.instances.either._
import cats.instances.list._
import cats.syntax.traverse._
import io.circe
import io.circe.parser._
import io.circe._
import io.circe.generic.semiauto._
import cats.syntax.either._
import org.scalatest.EitherValues
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scalaj.http.Http

import java.time.format.DateTimeFormatter

class HomeworkSpec extends AnyWordSpec with Matchers with EitherValues {
  import HomeworkSpec._

  "NBA JSON API client" should {
    "get info about today games" in {
      val date = LocalDate.now()
      val scoreboardOrError = fetchScoreboard(date)
      val scoreboard =
        scoreboardOrError.getOrElse(fail(scoreboardOrError.toString))
      val allGameIds = scoreboard.games.map(_.gameId)
      val gameInfosOrError = allGameIds.map(fetchGameInfo(date, _)).sequence
      gameInfosOrError.getOrElse(fail(gameInfosOrError.toString))
      succeed
    }

    "fetch games for 14 Feb 2020" in {
      val date = LocalDate.of(2020, 2, 14)
      val scoreboardOrError = fetchScoreboard(date)
      val scoreboard =
        scoreboardOrError.getOrElse(fail(scoreboardOrError.toString))
      val allGameIds = scoreboard.games.map(_.gameId)
      val gameInfosOrError = allGameIds.map(fetchGameInfo(date, _)).sequence
      val gameInfos =
        gameInfosOrError.getOrElse(fail(gameInfosOrError.toString))
      gameInfos.size must be(1)
    }
  }
}

object HomeworkSpec {

  val dataFormat: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyyMMdd")
  implicit val dateDecoder: Decoder[LocalDate] =
    Decoder.decodeString.emap(str => {
      Either
        .catchNonFatal(LocalDate.parse(str, dataFormat))
        .leftMap(_.getMessage)
    })

  final case class TeamTotals(
      assists: String,
      fullTimeoutRemaining: String,
      plusMinus: String
  )
  implicit val decoderTeamTotals: Decoder[TeamTotals] = hcursor => {
    for {
      assists <- hcursor.downField("assists").as[String]
      fullTimeoutRemaining <-
        hcursor.downField("full_timeout_remaining").as[String]
      plusMinus <- hcursor.downField("plusMinus").as[String]
    } yield TeamTotals(assists, fullTimeoutRemaining, plusMinus)
  }
  final case class TeamBoxScore(totals: TeamTotals)
  implicit val decoderTeamBoxScore: Decoder[TeamBoxScore] =
    deriveDecoder[TeamBoxScore]

  final case class GameStats(hTeam: TeamBoxScore, vTeam: TeamBoxScore)
  implicit val decoderGameStats: Decoder[GameStats] = deriveDecoder[GameStats]

  final case class PrevMatchup(gameDate: LocalDate, gameId: String)
  implicit val decoderPrevMatchup: Decoder[PrevMatchup] =
    deriveDecoder[PrevMatchup]

  final case class BoxScore(
      basicGameData: Game,
      previousMatchup: PrevMatchup,
      stats: Option[GameStats]
  )
  implicit val decoderBoxScore: Decoder[BoxScore] = deriveDecoder[BoxScore]

  final case class JustScore(score: String)
  implicit val decoderJustScore: Decoder[JustScore] = deriveDecoder[JustScore]

  final case class TeamStats(
      linescore: List[JustScore],
      loss: String,
      score: String,
      teamId: String,
      triCode: String
  )
  implicit val decoderTeamStats: Decoder[TeamStats] = deriveDecoder[TeamStats]

  final case class GameDuration(hours: String, minutes: String)
  implicit val decoderGameDuration: Decoder[GameDuration] =
    deriveDecoder[GameDuration]

  final case class Arena(
      city: String,
      country: String,
      isDomestic: Boolean,
      name: String,
      stateAbbr: String
  )
  implicit val decoderArena: Decoder[Arena] = deriveDecoder[Arena]

  final case class Game(
      arena: Arena,
      attendance: String,
      endTimeUTC: Option[ZonedDateTime],
      gameDuration: GameDuration,
      gameId: String,
      gameUrlCode: String,
      hTeam: TeamStats,
      isBuzzerBeater: Boolean,
      startTimeUTC: ZonedDateTime,
      vTeam: TeamStats
  )
  implicit val decoderGame: Decoder[Game] = deriveDecoder[Game]

  final case class Scoreboard(games: List[Game], numGames: Int)
  implicit val decoderScoreboard: Decoder[Scoreboard] =
    deriveDecoder[Scoreboard]

//
  //

  private def fetchScoreboard(
      date: LocalDate
  ): Either[circe.Error, Scoreboard] = {
    val dateString = date.format(DateTimeFormatter.BASIC_ISO_DATE)
    val body = Http(
      s"https://data.nba.net/10s/prod/v1/$dateString/scoreboard.json"
    ).asString.body
    decode[Scoreboard](body)
  }

  private def fetchGameInfo(
      date: LocalDate,
      gameId: String
  ): Either[circe.Error, BoxScore] = {
    val dateString = date.format(DateTimeFormatter.BASIC_ISO_DATE)
    val body = Http(
      s"https://data.nba.net/10s/prod/v1/$dateString/${gameId}_boxscore.json"
    ).asString.body
    decode[BoxScore](body)
  }
}
