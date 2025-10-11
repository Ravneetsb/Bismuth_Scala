package bismuth
import fastparse._
import Expr._
class IdentifierTest extends munit.FunSuite {
  test("reserved words: red") {
    val input = "red"
    val result = parse(input, Parser.identifier(using _))
    result match {
      case Parsed.Success(_, _) =>
        fail("Parser should reject reserved words like 'red'")
      case Parsed.Failure(_, _, _) =>
        assert(true) // expected failure
    }
  }

  test("reserved words: if") {
    val input = "if"
    val result = parse(input, Parser.identifier(using _))
    result match {
      case Parsed.Success(_, _) =>
        fail("Parser should reject reserved words like 'if'")
      case Parsed.Failure(_, _, _) =>
        assert(true) // expected failure
    }
  }
  test("reserved words: true") {
    val input = "true"
    val result = parse(input, Parser.identifier(using _))
    result match {
      case Parsed.Success(_, _) =>
        fail("Parser should reject reserved words like 'true'")
      case Parsed.Failure(_, _, _) =>
        assert(true) // expected failure
    }
  }

  test("idenitifier") {
    val input = "pop"
    val result = parse(input, Parser.identifier(using _))
    result match {
      case Parsed.Success(_, _) =>
        assert(true)
      case Parsed.Failure(_, _, _) =>
        fail("Parser should parse non-reserved words as identifiers")
    }
  }

}
