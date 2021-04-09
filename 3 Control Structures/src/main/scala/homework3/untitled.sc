import scala.io.Source

object Control_structures {

  sealed trait Command
  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    //  final case class Sum(numbers: List[Double])                extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
  }

  final case class ErrorMessage(value: String)

  sealed trait Result
  final case class ChangeMe(value: String) extends Result

  /*def parseCommand(x: String): Either[ErrorMessage, Command] =

  def calculate(x: Command): Either[ErrorMessage, Result] =

  def renderResult(x: Result): String =
   */

  def process(x: String): String = { x }

  def main(args: Array[String]): Unit =
    for (str <- Source.stdin.getLines() map { line => process(line) })
      println(str)
}
