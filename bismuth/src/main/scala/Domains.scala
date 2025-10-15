package bismuth

enum Value:
  case ColorV(im: ImageColor)
  case GrayV(im: ImageGray)
  case MaskV(im: ImageMask)

object Value:
  def liftVPoly(f: [A] => Image[A] => Image[A])(v: Value): Value = v match
    case Value.ColorV(im) => Value.ColorV(f[Color](im))
    case Value.GrayV(im)  => Value.GrayV(f[Double](im))
    case Value.MaskV(im)  => Value.MaskV(f[Boolean](im))

  trait ValueLike[A]:
    def toValue(im: Image[A]): Value
    def fromValue(v: Value): Option[Image[A]]

  given colorValueLike: ValueLike[Color] with
    def toValue(im: Image[Color]): Value = Value.ColorV(im)
    def fromValue(v: Value): Option[Image[Color]] = v match
      case Value.ColorV(im) => Some(im)
      case _                => None

  given grayValueLike: ValueLike[Double] with
    def toValue(im: Image[Double]): Value = Value.GrayV(im)
    def fromValue(v: Value): Option[Image[Double]] = v match
      case Value.GrayV(im) => Some(im)
      case _               => None

  given maskValueLike: ValueLike[Boolean] with
    def toValue(im: Image[Boolean]): Value = Value.MaskV(im)
    def fromValue(v: Value): Option[Image[Boolean]] = v match
      case Value.MaskV(im) => Some(im)
      case _               => None

  def liftV2(
      f: [A] => (Image[A], Image[A]) => Image[A]
  )(v1: Value)(v2: Value): Option[Value] =
    (v1, v2) match
      case (Value.ColorV(im1), Value.ColorV(im2)) =>
        Some(Value.ColorV(f[Color](im1, im2)))
      case (Value.GrayV(im1), Value.GrayV(im2)) =>
        Some(Value.GrayV(f[Double](im1, im2)))
      case (Value.MaskV(im1), Value.MaskV(im2)) =>
        Some(Value.MaskV(f[Boolean](im1, im2)))
      case _ => None

  def liftVs(
      f: [A] => List[Image[A]] => Image[A]
  )(values: List[Value]): Option[Value] =
    values match
      case Nil => None
      case head :: tail =>
        head match
          case Value.ColorV(im1) =>
            val ims = values.collect { case Value.ColorV(im) => im }
            if ims.size == values.size then Some(Value.ColorV(f[Color](ims)))
            else None
          case Value.GrayV(im1) =>
            val ims = values.collect { case Value.GrayV(im) => im }
            if ims.size == values.size then Some(Value.GrayV(f[Double](ims)))
            else None
          case Value.MaskV(im1) =>
            val ims = values.collect { case Value.MaskV(im) => im }
            if ims.size == values.size then Some(Value.MaskV(f[Boolean](ims)))
            else None
          case _ => None

type ArithValue = Double

type Env = Map[Var, Value]
