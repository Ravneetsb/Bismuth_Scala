package bismuth

import fastparse._, NoWhitespace._
import Expr.*

object Parser {
  val namedColors: Map[String, Color] = Map(
    "red" -> Color.red,
    "green" -> Color.green,
    "blue" -> Color.blue,
    "yellow" -> Color.yellow,
    "black" -> Color.black,
    "white" -> Color.white
  )

  val reserved: Set[String] = Set(
    "true",
    "false",
    "x",
    "y",
    "r",
    "theta",
    "scale",
    "translate",
    "rotate",
    "swirl",
    "juxtapose",
    "if",
    "else",
    "replicate",
    "flip",
    "overlay",
    "sin",
    "cos",
    "tan",
    "sqrt"
  ) ++ namedColors.keySet

  def namedColor(using p: P[?]): P[Expr] = P(
    StringIn("red", "green", "blue", "yellow", "black", "white").!
  ).map(c => ColorLit(namedColors(c)))

  @main def run(): Unit =
    val result = parse("red", namedColor(using _))
    result match
      case Parsed.Success(value, successIndex) =>
        println(s"Success! value=$value, index=$successIndex")
      case f: Parsed.Failure =>
        println(s"Failed: $f")
}
