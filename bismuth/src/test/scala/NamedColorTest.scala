// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
package bismuth
import fastparse._
import Expr._
class NamedColorTest extends munit.FunSuite {
  for ((name, col) <- Parser.namedColors) {
    test(name) {
      val result = parse(name, Parser.namedColor(using _))
      result match
        case Parsed.Success(value, successIndex) =>
          assertEquals(value, Expr.ColorLit(col))
        case f: Parsed.Failure =>
          println(s"Failed: $f")
    }
  }
}
