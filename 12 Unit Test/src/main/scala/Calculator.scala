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

  def plus: Calculator = this.copy(operation = Some(Operation.Plus))
  def minus: Calculator = this.copy(operation = Some(Operation.Minus))
  def multiply: Calculator = this.copy(operation = Some(Operation.Multiply))
  def divide: Calculator = this.copy(operation = Some(Operation.Divide))

  def calculate(calculator: Calculator): Either[String, Calculator] =
    calculator.operation match {
      case Some(Operation.Plus) =>
        Right(
          Calculator(memory = 0, screen = calculator.screen + calculator.memory)
        )
      case Some(Operation.Minus) =>
        Right(
          Calculator(memory = 0, screen = calculator.memory - calculator.screen)
        )
      case Some(Operation.Multiply) =>
        if (
          Int.MaxValue > calculator.screen * calculator.memory &&
          Int.MinValue < calculator.screen * calculator.memory
        )
          Right(
            Calculator(
              memory = 0,
              screen = calculator.screen * calculator.memory
            )
          )
        else Left("vary big or small digital")
      case Some(Operation.Divide) =>
        if (calculator.screen != 0)
          Right(
            Calculator(
              memory = 0,
              screen = calculator.memory / calculator.screen
            )
          )
        else Left("Dont can divide on 0")
      case _ => Left("This command not exist")
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
