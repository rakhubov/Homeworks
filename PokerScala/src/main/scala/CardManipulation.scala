import RequestInDB._
import cats.effect.IO
import doobie.Transactor

import java.util.UUID
import scala.util.Random

object CardManipulation {
  val catalyst = 55
  val cardInDeck = 54

  def generationCard(
      numberCard: Int,
      acc: Set[Int] = Set(catalyst)
  ): Set[Int] = {
    if (acc.size < numberCard + 1)
      generationCard(numberCard, acc.incl(Random.nextInt(cardInDeck)))
    else acc.excl(catalyst)
  }

  def writePlayerCard(
      cardTable: List[Int],
      allCardInHands: List[Int],
      playersID: List[String],
      connectToDataBase: Transactor[IO]
  ): IO[Unit] =
    if (allCardInHands.size > 1) {

      val playerID = Option(
        UUID.fromString(playersID.head)
      ) match {
        case Some(value) => value
        case _           => UUID.randomUUID()
      }
      val oneCard = allCardInHands.head.toString()
      val twoCard = oneCard + ' ' + allCardInHands.tail.head.toString
      val stringTablePlayerCard = cardTable match {
        case c1 :: c2 :: c3 :: c4 :: c5 :: Nil =>
          c1.toString + ' ' +
            c2.toString + ' ' + c3.toString + ' ' +
            c4.toString + ' ' + c5.toString + ' ' + twoCard
        case _ => ""
      }

      for {

        _ <- writeAllPlayerCard(stringTablePlayerCard, twoCard, playerID)
          .transact(connectToDataBase)

      } yield IO(Unit)

      writePlayerCard(
        cardTable,
        allCardInHands.takeRight(allCardInHands.size - 2),
        playersID.tail,
        connectToDataBase
      )
    }

}
