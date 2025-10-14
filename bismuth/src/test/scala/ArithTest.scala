package bismuth
import fastparse._
import Expr._
class ArithTest extends munit.FunSuite {
  test("arithAtom") {
    val inputs = Array("pi", "15")

    for (input <- inputs) {
      val result = parse(input, Parser.arithAtom(using _))
      result match
        case Parsed.Success(_, _) =>
          assert(true)
        case Parsed.Failure(_, _, _) =>
          fail(s"Parser failed on input: '$input'")
    }
  }

  test("arithUnop") {
    val inputs = Array("-12", "12")

    for (input <- inputs) {
      val result = parse(input, Parser.arithUnop(using _))
      result match
        case Parsed.Success(_, _) =>
          assert(true)
        case Parsed.Failure(_, _, _) =>
          fail(s"Parser failed on input: '$input'")
    }
  }

  test("arithMul") {
    val inputs = Array("2  *3", "4 / 4", " 2*34 ", "-1 * -23")
    for (input <- inputs) {
      val result = parse(input, Parser.arithMul(using _))
      result match
        case Parsed.Success(item, _) =>
          item match
            case Arith.BinOpA(_, _, _) => assert(true)
            case _ => fail(s"Parser failed on input: '$input'")
        case Parsed.Failure(_, _, _) =>
          fail(s"Parser failed on input: '$input'")
    }
  }

  test("arithExpr") {

    val inputs =
      Array("2 + sin(pi)", "sin(pi) +  cos(pi)", "1 + 2 -  4 * 2 / 4")
    for (input <- inputs) {
      val result = parse(input, Parser.arithMul(using _))
      result match
        case Parsed.Success(item, _) =>
          assert(true)
        case Parsed.Failure(_, _, _) =>
          fail(s"Parser failed on input: '$input'")
    }
  }
}
