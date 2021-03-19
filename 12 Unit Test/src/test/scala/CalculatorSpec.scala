import Calculator.Operation
import org.scalatest.funsuite.AnyFunSuite
class CalculatorSpec extends AnyFunSuite {


      test ("Calculator test"){
        val calculator = Calculator()
        assert(
          calculator.enter(1) == Right(Calculator(1, 0, None))
        )
        assert(calculator.enter(7) == Right(Calculator(7, 0, None)))
        assert(calculator.enter(12) == Left("digit out of range"))
      }

    }

