package bismuth

import Expr._
import Value.*

enum RunTimeError:
  case runTimeError(s: String)
  case parseError(s: String)

def err(s: String) = Left(RunTimeError.runTimeError(s))

def handleMaybe(s: String)(a: Option[Value]): Either[RunTimeError, Value] =
  a match {
    case Some(v) => Right(v)
    case None    => err(s)
  }

def eval(ρ: Env, e: Expr): Either[RunTimeError, Value] =
  e match {
    case ColorLit(c) => Right(ColorV(lift0(c)))
    case GrayLit(c)  => Right(GrayV(lift0(c)))
    case MaskLit(c)  => Right(MaskV(lift0(c)))
    case X           => Right(GrayV(Point.getX))
    case Y           => Right(GrayV(Point.getY))
    case R           => Right(GrayV(fromPolarIm((p) => p.r)))
    case Theta       => Right(GrayV(fromPolarIm((p) => p.theta)))
    case ColorE(r, g, b, a) =>
      for
        rv <- eval(ρ, r)
        gv <- eval(ρ, g)
        bv <- eval(ρ, b)
        av <- eval(ρ, a)
        result <- mkColor(rv)(gv)(bv)(av)
      yield result

    case VarE(x) => handleMaybe(x)(ρ.get(x))
    case Let(x, e1, e2) =>
      for
        v1 <- eval(ρ, e1)
        result <- eval(ρ + (x -> v1), e2)
      yield result

    case BinOp(e1, op, e2) =>
      for
        v1 <- eval(ρ, e1)
        v2 <- eval(ρ, e2)
        result <- evalBinOp(v1)(op)(v2)
      yield result

    case UnOp(op, e) =>
      for
        v <- eval(ρ, e)
        result <- evalUnOp(op)(v)
      yield result

    // case Flip(e) =>
    //   for
    //     v <- eval(ρ, e)
    //     result <- liftVPoly(flipV)(v)
    //   yield result
    case Flip(e) =>
      eval(ρ, e).map(v => liftVPoly(flipV)(v))

  }

def evalUnOp(op: bismuth.UnOp)(v: Value) =
  (op, v) match {
    case (bismuth.UnOp.Negate, GrayV(im)) => Right(GrayV(lift1D(-_)(im)))
    case (bismuth.UnOp.BuiltIn(f), GrayV(im)) =>
      Right(GrayV(lift1D(BuiltInFn.getFn(f))(im)))

    case (bismuth.UnOp.Negate, ColorV(im)) =>
      Right(
        ColorV(lift1C(c => c.copy(r = -c.r, g = -c.g, b = -c.b, a = c.a))(im))
      )
    case (bismuth.UnOp.BuiltIn(f), ColorV(im)) =>
      Right(ColorV(lift1C(c => c.cMap(BuiltInFn.getFn(f)))(im)))

    case (bismuth.UnOp.Not, MaskV(im)) =>
      Right(MaskV(lift1B(!_)(im)))

    case (op, _) => err(s"Unary operator and operand mismatch. op: $op")

  }

def lift1B = lift1[Boolean, Boolean]
def lift1D = lift1[Double, Double]
def lift1C = lift1[Color, Color]

def evalBinOp(v1: Value)(op: bismuth.BinOp)(v2: Value) =
  (v1, op, v2) match {
    case (GrayV(v1), bismuth.BinOp.Add, GrayV(v2)) =>
      Right(GrayV(lift2D(_ + _)(v1)(v2)))
    case (GrayV(v1), bismuth.BinOp.Sub, GrayV(v2)) =>
      Right(GrayV(lift2D(_ - _)(v1)(v2)))
    case (GrayV(v1), bismuth.BinOp.Div, GrayV(v2)) =>
      Right(GrayV(lift2D(_ / _)(v1)(v2)))
    case (GrayV(v1), bismuth.BinOp.Mul, GrayV(v2)) =>
      Right(GrayV(lift2D(_ * _)(v1)(v2)))
    case (GrayV(v1), bismuth.BinOp.Less, GrayV(v2)) =>
      Right(MaskV(lift2DB(_ < _)(v1)(v2)))
    case (GrayV(v1), bismuth.BinOp.Eq, GrayV(v2)) =>
      Right(MaskV(lift2DB(_ == _)(v1)(v2)))

    case (ColorV(v1), bismuth.BinOp.Add, ColorV(v2)) =>
      Right(ColorV(lift2C(_ + _)(v1)(v2)))
    case (ColorV(v1), bismuth.BinOp.Sub, ColorV(v2)) =>
      Right(ColorV(lift2C(_ - _)(v1)(v2)))
    case (ColorV(v1), bismuth.BinOp.Mul, ColorV(v2)) =>
      Right(ColorV(lift2C(_ * _)(v1)(v2)))
    case (ColorV(v1), bismuth.BinOp.Eq, ColorV(v2)) =>
      Right(MaskV(lift2CB(_ == _)(v1)(v2)))

    case (MaskV(v1), bismuth.BinOp.And, MaskV(v2)) =>
      Right(MaskV(lift2B(_ && _)(v1)(v2)))
    case (MaskV(v1), bismuth.BinOp.Or, MaskV(v2)) =>
      Right(MaskV(lift2B(_ || _)(v1)(v2)))
    case (MaskV(v1), bismuth.BinOp.Xor, MaskV(v2)) =>
      Right(MaskV(lift2B(_ != _)(v1)(v2)))
    case (MaskV(v1), bismuth.BinOp.Eq, MaskV(v2)) =>
      Right(MaskV(lift2B(_ == _)(v1)(v2)))

    case (_, op, _) => err(s"Binary Operator and Operand Mismatch. op: $op")
  }

def lift2D = lift2[Double, Double, Double]
def lift2DB = lift2[Double, Double, Boolean]
def lift2C = lift2[Color, Color, Color]
def lift2CB = lift2[Color, Color, Boolean]
def lift2B = lift2[Boolean, Boolean, Boolean]

def mkColor(
    r: Value
)(g: Value)(b: Value)(a: Value): Either[RunTimeError, Value] =
  (r, g, b, a) match {
    case (Value.GrayV(ri), Value.GrayV(gi), Value.GrayV(bi), Value.GrayV(ai)) =>
      Right(Value.ColorV(liftA4(Color.apply, ri, gi, bi, ai)))
    case _ =>
      err("Expected grayscale values in mkColor")
  }
