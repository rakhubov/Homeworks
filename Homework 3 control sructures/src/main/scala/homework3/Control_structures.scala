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
  final case class ChangeMe(command: Command, result: Double) extends Result {}

  def stringTOdouble(list: List[String]): Try[List[Double]] = {
    Try(list.map(x => x.toDouble))
  }

  import Command._
  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    x.split("\\s+").toList match {
      case "divide" :: figures if figures.length == 2 =>
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
      case _ => Left(ErrorMessage("input command " + x))
    }
  }

  def calculate(command: Command): Either[ErrorMessage, Result] =
    command match {
      case Divide(dividend, divisor) => Right(ChangeMe(Divide(dividend, divisor), (dividend / divisor)))
      case Sum(numbers) => Right(ChangeMe(Sum(numbers), (numbers.sum)))
      case Average(numbers) => Right(ChangeMe(Average(numbers), (numbers.sum / numbers.length)))
      case Max(numbers) => Right(ChangeMe(Max(numbers), numbers.max))
      case Min(numbers) => Right(ChangeMe(Min(numbers), numbers.min))
      case _            => Left(ErrorMessage(" dont can cancul "))
    }

  def renderResult(res: Result): String =
    res match {
      case ChangeMe(Divide(dividend, divisor), result) => s"$dividend divided by $divisor is $result"
      case ChangeMe(Sum(figures), result) => s"the sum of ${figures.mkString(" ")} is $result"
      case ChangeMe(Average(figures), result) => s"the average of ${figures.mkString(" ")} is $result"
      case ChangeMe(Min(figures), result) => s"the minimum of ${figures.mkString(" ")} is $result"
      case ChangeMe(Max(figures), result) => s"the maximum of ${figures.mkString(" ")} is $result"
    }

  def process(x: String): String = {
    val process = for {
      parce <- parseCommand(x)
      calc <- calculate(parce)
    } yield renderResult(calc)

    process match {
      case Right(value) => value
      case Left(error)  => error.value
    }
  }

  def main(args: Array[String]): Unit =
    for (str <- Source.stdin.getLines() map { line => process(line) })
      println(str)
}
