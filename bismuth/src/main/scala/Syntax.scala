package bismuth

import scala.math._

type Var = String

// Expressions

sealed trait Expr

object Expr {
  case class ColorLit(value: Color) extends Expr
  case class GrayLit(value: Double) extends Expr
  case class MaskLit(value: Boolean) extends Expr

  case object X extends Expr
  case object Y extends Expr
  case object R extends Expr
  case object Theta extends Expr

  case class VarE(name: Var) extends Expr

  case class BinOp(lhs: Expr, op: bismuth.BinOp, rhs: Expr) extends Expr
  case class UnOp(op: bismuth.UnOp, expr: Expr) extends Expr

  case class Let(name: Var, value: Expr, body: Expr) extends Expr

  case class ColorE(r: Expr, g: Expr, b: Expr, a: Expr) extends Expr
  // Transformations
  case class Flip(e: Expr) extends Expr
  case class Scale(a: Arith, e: Expr) extends Expr
  case class Swirl(a: Arith, e: Expr) extends Expr
  case class Scale2(ax: Arith, ay: Arith, e: Expr) extends Expr
  case class Translate(dx: Arith, dy: Arith, e: Expr) extends Expr
  case class Rotate(angle: Arith, e: Expr) extends Expr
  case class Overlay(e1: Expr, e2: Expr) extends Expr

  case class If(cond: Expr, thenBranch: Expr, elseBranch: Expr) extends Expr

  case class Juxtapose(first: Expr, rest: List[Expr]) extends Expr
  case class JuxtaposeH(left: Expr, right: Expr) extends Expr
  case class JuxtaposeV(top: Expr, bottom: Expr) extends Expr

  case class Replicate(times: Arith, e: Expr) extends Expr
}

enum BinOp:
  case Add, Mul, Div, Sub, Less, Eq, And, Or, Xor

enum UnOp:
  case Negate
  case BuiltIn(fn: BuiltInFn)
  case Not

// ===== Arithmetic expressions =====

sealed trait Arith

object Arith {
  case object Pi extends Arith
  case class BinOpA(lhs: Arith, op: bismuth.BinOpA, rhs: Arith) extends Arith
  case class UnOpA(op: bismuth.UnOpA, expr: Arith) extends Arith
  case class NumLit(value: Double) extends Arith
}

enum BinOpA:
  case AddA, MulA, DivA, SubA

enum UnOpA:
  case NegateA
  case BuiltInA(fn: BuiltInFn)

enum BuiltInFn:
  case Sin, Cos, Tan, Sqrt

object BuiltInFn:
  def getFn(fn: BuiltInFn): Double => Double = fn match
    case Sin  => math.sin
    case Cos  => math.cos
    case Tan  => math.tan
    case Sqrt => math.sqrt

case class Program(image: Expr, resolution: (Int, Int))
