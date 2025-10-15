package bismuth

import fastparse._
import fastparse.ScalaWhitespace.whitespace
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

  def identifier(using p: P[?]): P[String] =
    P(
      CharIn("a-zA-Z_") ~ CharsWhile(c => c.isLetterOrDigit || c == '\'', 0)
    ).!.map(_.trim).filter(
      !reserved.contains(_)
    )

  def number(using p: P[?]): P[Double] =
    P(CharIn("0-9").rep(1).! ~ ("." ~ CharIn("0-9").rep(1).!).?).map {
      case (i, None)    => i.toDouble
      case (i, Some(d)) => s"$i.$d".toDouble
    }

  def comma(using p: P[?]): P[Unit] =
    P(CharIn(","))

  def semicolon(using p: P[?]): P[Unit] =
    P(CharIn(";"))

  def wordBoundary(using p: P[?]): P[Unit] =
    P(!(CharIn("a-zA-Z0-9_")))

  // def kw(s: String)(using p: P[?]): P[Unit] =
  //   P(s ~ wordBoundary).!
  def kw(s: String)(using p: P[?]): P[Unit] =
    P(s ~~ !(CharIn("a-zA-Z0-9_")))

  def boolean(using p: P[?]): P[Expr] =
    P(
      kw("true").map(_ => MaskLit(true)) | kw("false").map(_ => MaskLit(false))
    )

  def keyword(using p: P[?]): P[Expr] =
    P(
      kw("x").map(_ => Expr.X) |
        kw("y").map(_ => Expr.Y) |
        (kw("r") ~ !CharIn("o", "e")).map(_ => Expr.R) |
        kw("theta").map(_ => Expr.Theta) |
        kw("pi").map(_ => Expr.GrayLit(math.Pi))
    )

  // def whitespace(using p: P[?]): P[Unit] = P(CharIn(" \n\r\t"))
  // TODO:  Correct usage->
  // val result = parse("(pi)", p => parens(using p)(keyword(using p)))
  // def parens(using p: P[?])(pa: => P[?]): P[?] = P("(" ~/ pa ~ ")")
  // def parens[A: P](p: => P[A]): P[A] = P("(" ~/ p ~ ")")
  def parens[A](parser: => P[A])(using P[?]): P[A] =
    P("(" ~/ parser ~ ")")

  def braces[A](parser: => P[A])(using P[?]): P[A] =
    P("{" ~/ parser ~ "}")

  // Arithmetic

  def builtInA(using p: P[?]): P[BuiltInFn] =
    P(StringIn("sin", "cos", "tan", "sqrt").!).map {
      case "sin"  => BuiltInFn.Sin
      case "cos"  => BuiltInFn.Cos
      case "tan"  => BuiltInFn.Tan
      case "sqrt" => BuiltInFn.Sqrt
    }

  def arithAtom(using p: P[?]): P[Arith] =
    P(
      kw("pi").map(_ => Arith.Pi) |
        number.map(Arith.NumLit.apply) |
        builtInA.flatMap(fn =>
          parens(arithExpr).map(a => Arith.UnOpA(UnOpA.BuiltInA(fn), a))
        ) |
        parens(arithExpr)
    )
  def arithUnop(using p: P[?]): P[Arith] =
    P("-".!.? ~ arithAtom).map {
      case (Some(_), a) => bismuth.Arith.UnOpA(bismuth.UnOpA.NegateA, a)
      case (None, a)    => a
    }

  def arithMul(using p: P[?]): P[Arith] =
    P(arithUnop ~ (CharIn("*/").! ~/ arithUnop).rep).map { case (base, ops) =>
      ops.foldLeft(base) {
        case (acc, ("*", rhs)) =>
          Arith.BinOpA(acc, BinOpA.MulA, rhs)
        case (acc, ("/", rhs)) =>
          Arith.BinOpA(acc, BinOpA.DivA, rhs)
      }
    }

  def arithExpr(using p: P[?]): P[Arith] =
    P(arithMul ~ (CharIn("+\\-").! ~/ arithMul).rep).map { case (base, ops) =>
      ops.foldLeft(base) {
        case (acc, ("+", rhs)) => Arith.BinOpA(acc, BinOpA.AddA, rhs)
        case (acc, ("-", rhs)) => Arith.BinOpA(acc, BinOpA.SubA, rhs)
      }
    }

  // Boolean

  def boolLit(using p: P[?]): P[Expr] =
    P(StringIn("true", "false").!).map {
      case "true"  => MaskLit(true)
      case "false" => MaskLit(false)
    }

  def grayLit(using p: P[?]): P[Expr] = number.map(GrayLit.apply)

  def colorLit(using p: P[?]): P[Expr] =
    P("[" ~/ expr ~ "," ~ expr ~ "," ~ expr ~ ("," ~ expr).? ~ "]").map {
      case (r, g, b, Some(a)) => ColorE(r, g, b, a)
      case (r, g, b, None)    => ColorE(r, g, b, GrayLit(1.0))
    }

  // Vars

  def coordVar(using p: P[?]): P[Expr] =
    P(StringIn("x", "y", "r", "theta").! ~ !CharIn("o", "e")).map {
      case "x"     => Expr.X
      case "y"     => Expr.Y
      case "r"     => Expr.R
      case "theta" => Expr.Theta
    }

  def varE(using p: P[?]): P[Expr] =
    identifier.map(Expr.VarE.apply)

  // Unary and Binary exps

  def simpleExpr(using p: P[?]): P[Expr] =
    P(
      varE |
        namedColor |
        colorLit |
        coordVar |
        (builtInA ~ parens(expr)).map((trig, e) =>
          bismuth.Expr.UnOp(bismuth.UnOp.BuiltIn(trig), e)
        ) |
        parens(expr) |
        braces(expr) |
        grayLit |
        boolLit |
        (kw("flip") ~ parens(expr)).map { case (e) => Expr.Flip(e) } |
        (kw("scale") ~ parens(arithExpr ~ comma ~ (arithExpr ~ comma).? ~ expr))
          .map {
            case (a, None, e) =>
              Expr.Scale(a, e)
            case (a1, Some(a2), e) => Expr.Scale2(a1, a2, e)
          } |
        (kw("swirl") ~ parens(arithExpr ~ comma ~ expr)).map { case (a, e) =>
          Expr.Swirl(a, e)
        } |
        (kw("rotate") ~ parens(arithExpr ~ comma ~ expr)).map { case (a, e) =>
          Expr.Rotate(a, e)
        } |
        (kw("replicate") ~ parens(arithExpr ~ comma ~ expr)).map {
          case (a, e) =>
            Expr.Replicate(a, e)
        } |
        (kw("overlay") ~ parens(expr ~ comma ~ expr)).map { case (e1, e2) =>
          Expr.Overlay(e1, e2)
        } |
        (kw("translate") ~ parens(arithExpr ~ comma ~ arithExpr ~ comma ~ expr))
          .map { case (ax, ay, e) =>
            Expr.Translate(ax, ay, e)
          } |
        (kw("juxtapose") ~ parens(expr.rep(1, sep = ","./)).map(exprs =>
          Expr.Juxtapose(exprs.head, exprs.tail.toList)
        )) |
        (kw("if") ~ expr ~ braces(expr) ~ kw("else") ~ braces(expr))
          .map((cond, t, f) => Expr.If(cond, t, f))
    )

  def unOpExpr(using p: P[?]): P[Expr] =
    P(("-".!).? ~ simpleExpr).map {
      case (Some(_), e) => Expr.UnOp(bismuth.UnOp.Negate, e)
      case (None, e)    => e
    }

  def mulDivExpr(using p: P[?]): P[Expr] =
    P(unOpExpr ~ (CharIn("*/").! ~/ unOpExpr).rep).map { case (base, ops) =>
      ops.foldLeft(base) {
        case (acc, ("*", rhs)) => Expr.BinOp(acc, bismuth.BinOp.Mul, rhs)
        case (acc, ("/", rhs)) => Expr.BinOp(acc, bismuth.BinOp.Div, rhs)
      }
    }

  def addSubExpr(using p: P[?]): P[Expr] =
    P(mulDivExpr ~ (CharIn("+\\-").! ~/ mulDivExpr).rep).map {
      case (base, ops) =>
        ops.foldLeft(base) {
          case (acc, ("+", rhs)) => Expr.BinOp(acc, bismuth.BinOp.Add, rhs)
          case (acc, ("-", rhs)) => Expr.BinOp(acc, bismuth.BinOp.Sub, rhs)
        }
    }

  def relExpr(using p: P[?]): P[Expr] =
    P(
      addSubExpr ~ (StringIn(">", "<", "==").! ~ !StringIn(
        "..",
        ":"
      ) ~/ addSubExpr).rep
    ).map {
      case (base, ops) => {
        ops.foldLeft(base) {
          case (acc, ("<", rhs))  => Expr.BinOp(acc, bismuth.BinOp.Less, rhs)
          case (acc, (">", rhs))  => Expr.BinOp(rhs, bismuth.BinOp.Less, acc)
          case (acc, ("==", rhs)) => Expr.BinOp(acc, bismuth.BinOp.Eq, rhs)
        }
      }
    }

  def negExpr(using p: P[?]): P[Expr] =
    P("!".!.? ~ relExpr).map {
      case (Some(_), a) => Expr.UnOp(bismuth.UnOp.Negate, a)
      case (None, a)    => a
    }

  def xOrExpr(using p: P[?]): P[Expr] =
    P(negExpr ~ (StringIn("^").! ~/ negExpr).rep).map {
      case (base, ops) => {
        ops.foldLeft(base) { case (acc, ("^", rhs)) =>
          Expr.BinOp(acc, bismuth.BinOp.Xor, rhs)
        }
      }
    }

  def andExpr(using p: P[?]): P[Expr] =
    P(xOrExpr ~ (StringIn("&&").! ~/ xOrExpr).rep).map {
      case (base, ops) => {
        ops.foldLeft(base) { case (acc, ("&&", rhs)) =>
          Expr.BinOp(acc, bismuth.BinOp.And, rhs)
        }
      }
    }
  def orExpr(using p: P[?]): P[Expr] =
    P(andExpr ~ (StringIn("||").! ~/ andExpr).rep).map {
      case (base, ops) => {
        ops.foldLeft(base) { case (acc, ("||", rhs)) =>
          Expr.BinOp(acc, bismuth.BinOp.Or, rhs)
        }
      }
    }
  def juxExpr(using p: P[?]): P[Expr] =
    P(orExpr ~ (StringIn("<..>", "<:>").! ~/ orExpr).rep).map {
      case (base, ops) => {
        ops.foldLeft(base) {
          case (acc, ("<..>", rhs)) =>
            Expr.JuxtaposeH(acc, rhs)
          case (acc, ("<:>", rhs)) =>
            Expr.JuxtaposeV(acc, rhs)
        }
      }
    }

  def exprOp(using p: P[?]): P[Expr] =
    P(juxExpr)

  def letExpr(using p: P[?]): P[Expr] =
    P(identifier ~ "=" ~ expr ~ ";" ~ expr)
      .map((name, value, body) => Expr.Let(name, value, body))

  def ifExpr(using p: P[?]): P[Expr] =
    P(kw("if") ~/ expr ~ kw("then") ~ expr ~ kw("else") ~ expr)
      .map((cond, thenB, elseB) => If(cond, thenB, elseB))
  def expr(using p: P[?]): P[Expr] =
    P(letExpr | exprOp)

  def program(using p: P[?]): P[Program] =
    P(parens(number ~ "," ~ number) ~ ";" ~ expr)
      .map((w, h, e) => Program(e, (w.toInt, h.toInt)))

  def parseProgram(input: String): Parsed[Program] =
    parse(input, p => program(using p))

  @main def run(): Unit =
    val result =
      parse(
        "(64, 64); a = 0.2; a",
        p => program(using p)
      )
    result match
      case Parsed.Success(value, successIndex) =>
        println(s"Success! value=$value, index=$successIndex")
      case f: Parsed.Failure =>
        println(s"Failed: $f")
}
