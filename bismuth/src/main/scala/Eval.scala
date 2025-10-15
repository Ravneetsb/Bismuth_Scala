package bismuth

import Expr._
import Value.*
import java.awt.image.BufferedImage

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

    case VarE(x) => {
      handleMaybe(s"Did not find $x in Env")(ρ.get(x))
    }
    case Let(x, e1, e2) =>
      for
        v1 <- eval(ρ, e1)
        _ = println(s"Bound $x -> $v1")
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

    case Flip(e) =>
      for
        v <- eval(ρ, e)
        result = liftVPoly([A] => (im: Image[A]) => flipV(im))(v)
      yield result

    case Translate(dx, dy, e) =>
      for
        v <- eval(ρ, e)
        x = evalArith(dx)
        y = evalArith(dy)
        vec = Vector(x, y)
        result = liftVPoly([A] => (im: Image[A]) => translateIm(vec)(im))(v)
      yield result

    case Scale(a, e) =>
      for
        v <- eval(ρ, e)
        α = evalArith(a)
        result = liftVPoly([A] => (im: Image[A]) => scaleIm(α)(im))(v)
      yield result

    case Scale2(dx, dy, e) =>
      for
        v <- eval(ρ, e)
        x = evalArith(dx)
        y = evalArith(dy)
        result = liftVPoly([A] => (im: Image[A]) => scaleIm2(x, y)(im))(v)
      yield result

    case Rotate(θ, e) =>
      for
        v <- eval(ρ, e)
        α = evalArith(θ)
        result = liftVPoly([A] => (im: Image[A]) => rotateIm(α)(im))(v)
      yield result

    case Swirl(θ, e) =>
      for
        v <- eval(ρ, e)
        α = evalArith(θ)
        result = liftVPoly([A] => (im: Image[A]) => swirlIm(α)(im))(v)
      yield result

    case Overlay(e1, e2) =>
      for
        v1 <- eval(ρ, e1)
        v2 <- eval(ρ, e2)
        result <- (v1, v2) match {
          case (ColorV(v1), ColorV(v2)) =>
            Right(ColorV(overlayImage(v1)(v2)))
          case _ => err("Expected 2 ColorV values in overlay")
        }
      yield result

    case If(ge, te, fe) => {
      for
        g <- eval(ρ, ge)
        t <- eval(ρ, te)
        f <- eval(ρ, fe)
        result <- g match {
          case MaskV(g) =>
            handleMaybe("Expected images of the same type")(
              liftV2(
                [A] => (im1: Image[A], im2: Image[A]) => select(g)(im1)(im2)
              )(t)(f)
            )
          case _ => err("If condition must be a mask")
        }
      yield result
    }

    case JuxtaposeH(left, right) =>
      for
        l <- eval(ρ, left)
        r <- eval(ρ, right)

        result <- handleMaybe(
          "Expected Images to be of the same type in JuxtaposeH"
        )(
          liftV2(
            [A] =>
              (im1: Image[A], im2: Image[A]) => horizontalJuxtapose(im1)(im2)
          )(l)(r)
        )
      yield result

  }

def evalArith(e: Arith): Double =
  e match {
    case Arith.Pi                                => math.Pi
    case (Arith.NumLit(d))                       => d
    case (Arith.UnOpA(bismuth.UnOpA.NegateA, a)) => -evalArith(a)
    case (Arith.UnOpA(bismuth.UnOpA.BuiltInA(f), a)) =>
      BuiltInFn.getFn(f)(evalArith(a))
    case (Arith.BinOpA(a1, op, a2)) =>
      op match {
        case bismuth.BinOpA.AddA => evalArith(a1) + evalArith(a2)
        case bismuth.BinOpA.SubA => evalArith(a1) - evalArith(a2)
        case bismuth.BinOpA.MulA => evalArith(a1) * evalArith(a2)
        case bismuth.BinOpA.DivA => evalArith(a1) / evalArith(a2)
      }
  }

def evalUnOp(op: bismuth.UnOp)(v: Value) =
  (op, v) match {
    case (bismuth.UnOp.Negate, GrayV(im)) => Right(GrayV(lift1D(-_)(im)))
    case (bismuth.UnOp.BuiltIn(f), GrayV(im)) =>
      Right(GrayV(lift1(BuiltInFn.getFn(f))(im)))

    case (bismuth.UnOp.Negate, ColorV(im)) =>
      Right(
        ColorV(
          lift1((c: Color) => c.copy(r = -c.r, g = -c.g, b = -c.b, a = c.a))(im)
        )
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

def run(program: Program): Either[RunTimeError, BufferedImage] =
  eval(Map.empty, program.image).map { v =>
    def renderV(value: Value, res: (Int, Int)): BufferedImage = value match {
      case ColorV(im) => render(im, res._1, res._2)
      case GrayV(im)  => render(im, res._1, res._2)
      case MaskV(im)  => render(im, res._1, res._2)
    }

    renderV(v, program.resolution)
  }
