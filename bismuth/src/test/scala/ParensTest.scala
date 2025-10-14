package bismuth
import fastparse._
import Expr._
class ParensTest extends munit.FunSuite {
  test("Keywords: pi") {
    val input = "(pi)"
    val result =
      parse(input, p => Parser.parens(Parser.keyword(using p))(using p))
    result match
      case Parsed.Success(_, _) =>
        assert(true)
      case Parsed.Failure(_, _, _) =>
        fail("parens should be parsed.")
  }

  test("Keywords: theta") {
    val input = "(theta)"
    val result =
      parse(input, p => Parser.parens(Parser.keyword(using p))(using p))
    result match {
      case Parsed.Success(_, _) =>
        assert(true)
      case Parsed.Failure(_, _, _) =>
        fail("parens should be parsed.")
    }
  }
  test("Arithmetic: sin") {
    val input = "(sin)"
    val result =
      parse(input, p => Parser.parens(Parser.builtInA(using p))(using p))
    result match {
      case Parsed.Success(_, _) =>
        assert(true)
      case Parsed.Failure(_, _, _) =>
        fail("parens should be parsed.")
    }
  }
}
