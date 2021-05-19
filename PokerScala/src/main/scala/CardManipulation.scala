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

}
