
import Calculator._

/** Simple calculator with buttons.
 *
 * @param memory whatever is stored in the memory.
 * @param screen whatever you see on the screen.
 */
case class Calculator(
                       memory: Double = 0,
                       screen: Double = 0,
                       operation: Option[Operation] = None
                     ) {

  def enter(digit: Int): Either[String, Calculator] =
    if (digit >= 0 && digit <= 9) {
      Right(this.copy(memory = memory * 10 + digit))
    } else {
      Left("digit out of range")
    }

  def plus: Calculator = this.copy(operation = Some(Operation.Plus))
  def minus: Calculator = this.copy(operation = Some(Operation.Minus))
  def multiply: Calculator = this.copy(operation = Some(Operation.Multiply))
  def divide: Calculator = this.copy(operation = Some(Operation.Divide))

  def calculate(calculator: Calculator): Either[String, Calculator] =
    calculator.operation match{
      case Some(Operation.Plus) => Right(Calculator(memory = 0, screen = screen + memory))
      case Some(Operation.Minus) => Right(Calculator(memory = 0, screen =  memory - screen))
      case Some(Operation.Multiply) =>if(Double.MaxValue > calculator.screen * calculator.memory &&
        Double.MinValue < calculator.screen * calculator.memory) Right(Calculator(memory = 0, screen = screen * memory))
      else Left("vary big or small digital for Doubly")
      case Some(Operation.Divide) => if(calculator.screen != 0) Right(Calculator(memory = 0, screen =  memory / screen))
      else Left("Dont can divide on 0")
      case _ => Left("This command dont exist")
    }

}
object Calculator {
  sealed trait Operation
  object Operation {
    object Plus extends Operation
    object Minus extends Operation
    object Multiply extends Operation
    object Divide extends Operation
  }
}