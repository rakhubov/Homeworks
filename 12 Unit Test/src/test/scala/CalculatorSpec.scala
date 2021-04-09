import Calculator.Operation
import org.scalatest.funsuite.AnyFunSuite

class CalculatorSpec extends AnyFunSuite {

  test("Test calculator plus") {
    val calculator = Calculator()
    assert(
      calculator.calculate(Calculator(23, 12).plus) == Right(Calculator(0, 35)),
      "Error + and +"
    )
    assert(
      calculator.calculate(Calculator(23, -12).plus) == Right(
        Calculator(0, 11)
      ),
      "Error + and -"
    )
    assert(
      calculator.calculate(Calculator(-23, 12).plus) == Right(
        Calculator(0, -11)
      ),
      "Error - and +"
    )
    assert(
      calculator.calculate(Calculator(-23, -12).plus) == Right(
        Calculator(0, -35)
      ),
      "Error - and -"
    )
  }

  test("Test calculator minus") {
    val calculator = Calculator()
    assert(
      calculator.calculate(Calculator(23, 12).minus) == Right(
        Calculator(0, 11)
      ),
      "Error + and +"
    )
    assert(
      calculator.calculate(Calculator(23, -12).minus) == Right(
        Calculator(0, 35)
      ),
      "Error + and -"
    )
    assert(
      calculator.calculate(Calculator(-23, 12).minus) == Right(
        Calculator(0, -35)
      ),
      "Error - and +"
    )
    assert(
      calculator.calculate(Calculator(-23, -12).minus) == Right(
        Calculator(0, -11)
      ),
      "Error - and -"
    )
  }

  test("Test calculator multiply") {
    val calculator = Calculator()
    assert(
      calculator.calculate(
        Calculator(
          933488888,
          943389999
        ).multiply
      ) == Left(
        "vary big or small digital"
      )
    )
    assert(
      calculator.calculate(
        Calculator(
          -933488888,
          -943389999
        ).multiply
      ) == Left(
        "vary big or small digital"
      )
    )
    assert(
      calculator.calculate(Calculator(10, 12).multiply) == Right(
        Calculator(0, 120)
      ),
      "Error + and +"
    )
    assert(
      calculator.calculate(Calculator(10, -12).multiply) == Right(
        Calculator(0, -120)
      ),
      "Error + and -"
    )
    assert(
      calculator.calculate(Calculator(-10, 12).multiply) == Right(
        Calculator(0, -120)
      ),
      "Error - and +"
    )
    assert(
      calculator.calculate(Calculator(-10, -12).multiply) == Right(
        Calculator(0, 120)
      ),
      "Error - and -"
    )
  }

  test("Test calculator divide") {
    val calculator = Calculator()
    assert(
      calculator.calculate(
        Calculator(
          9,
          0
        ).divide
      ) == Left("Dont can divide on 0")
    )
    assert(
      calculator.calculate(Calculator(30, 3).divide) == Right(
        Calculator(0, 10)
      ),
      "Error + and +"
    )
    assert(
      calculator.calculate(Calculator(30, -2).divide) == Right(
        Calculator(0, -15)
      ),
      "Error + and -"
    )
    assert(
      calculator.calculate(Calculator(-10, 5).divide) == Right(
        Calculator(0, -2)
      ),
      "Error - and +"
    )
    assert(
      calculator.calculate(Calculator(-100, -10).divide) == Right(
        Calculator(0, 10)
      ),
      "Error - and -"
    )
  }

  test("Test calculator command") {
    val calculator = Calculator()
    assert(
      calculator.calculate(
        Calculator(
          9,
          10
        )
      ) == Left("This command not exist")
    )
  }
}
