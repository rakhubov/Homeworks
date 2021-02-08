package homework3
import scala.util.{Failure, Success, Try}
import scala.io.Source

object Control_structures {

  sealed trait Command
  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
  }

  final case class ErrorMessage(value: String) {
    val errorM = "Error: " + value
  }

  sealed trait Result
  final case class ChangeMe(value: String) extends Result

  def stringTOdouble(list: List[String]): Try[List[Double]] = {
    Try(list.map(x => x.toDouble))
  }

  import Command._
  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    x.split("\\s+").toList match {
      case "divide" :: figures =>
        stringTOdouble(figures) match {
          case Success(value) if value(1) != 0 =>
            Right(Divide(value.head, value(1)))
          case Failure(error) => Left(ErrorMessage("divide" + error.toString))
        }
      case "sum" :: figures =>
        stringTOdouble(figures) match {
          case Success(value) => Right(Sum(value))
          case Failure(error) => Left(ErrorMessage("sum" + error.toString))
        }
      case "average" :: figures =>
        stringTOdouble(figures) match {
          case Success(value) => Right(Average(value))
          case Failure(error) => Left(ErrorMessage("average" + error.toString))
        }
      case "min" :: figures if figures.nonEmpty =>
        stringTOdouble(figures) match {
          case Success(value) => Right(Min(value))
          case Failure(error) => Left(ErrorMessage("min" + error.toString))
        }
      case "max" :: figures if figures.nonEmpty =>
        stringTOdouble(figures) match {
          case Success(value) => Right(Max(value))
          case Failure(error) => Left(ErrorMessage("max" + error.toString))
        }
      case _ => Left(ErrorMessage("input command"))
    }
  }

  def calculate(x: Command): Double =
    x match {
      case Divide(dividend, divisor) => dividend / divisor
      case Sum(numbers)              => numbers.sum
      case Average(numbers)          => numbers.sum / numbers.length
      case Max(numbers)              => numbers.max
      case Min(numbers)              => numbers.min
    }

  def renderResult(x: Command): String =
    x match {
      case Divide(dividend, divisor) =>
        s"$dividend divided by $divisor is ${calculate(x)}"
      case Sum(figures) =>
        s"the sum of ${figures.mkString(" ")} is ${calculate(x)}"
      case Average(figures) =>
        s"the average of ${figures.mkString(" ")} is ${calculate(x)}"
      case Min(figures) =>
        s"the minimum of ${figures.mkString(" ")} is ${calculate(x)}"
      case Max(figures) =>
        s"the maximum of ${figures.mkString(" ")} is ${calculate(x)}"
    }

  def process(x: String): String = {
    parseCommand(x) match {
      case Right(value) => renderResult(value)
      case Left(error)  => error.errorM
    }
  }

  def main(args: Array[String]): Unit =
    for (str <- Source.stdin.getLines() map { line => process(line) })
      println(str)
}
