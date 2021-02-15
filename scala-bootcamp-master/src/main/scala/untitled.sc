final case class GameLevel private (value: Int) extends AnyVal
object GameLevel {
  def create(value: Int): Option[GameLevel] =  {
    case 1 <= value <= 80 => Some(GameLevel(value))
    case _ => None
  }
}