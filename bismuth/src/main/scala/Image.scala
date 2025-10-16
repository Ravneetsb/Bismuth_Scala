package bismuth

import bismuth.Color
import bismuth.Point
import bismuth.Color.Grayscale
import bismuth.Color.gray

type Image[A] = Point => A

type PolarImage[A] = Polar => A

type ImageMask = Image[Boolean]
type ImageColor = Image[Color]
type ImageGray = Image[Grayscale]

type ImageT[A] = Image[A] => Image[A]

def lift0[A](value: A): Image[A] = _ => value

def lift1[A, B](f: A => B)(imageA: Image[A]): Image[B] =
  imageA.andThen(f)

def lift2[A, B, C](
    f: (A, B) => C
)(image1: Image[A])(image2: Image[B]): Image[C] = { (p: Point) =>
  f(image1(p), image2(p))
}

def lift3[A, B, C, D](
    f: (A, B, C) => D
)(imgA: Image[A])(imgB: Image[B])(imgC: Image[C]): Image[D] = { (p: Point) =>
  f(imgA(p), imgB(p), imgC(p))
}

// Interpolate Image
def interpolateImage(w: Double) = lift2(Color.interpolate(_, _, w))

// Overlay Image
def overlayImage = lift2(Color.overlay(_, _))

def select[A]: ImageMask => Image[A] => Image[A] => Image[A] =
  lift3((m, x, y) => if m then x else y)

def maskToGray(mask: ImageMask): ImageGray =
  (p: Point) => if mask(p) then Grayscale(0.0) else Grayscale(1.0)

def invert: ImageGray => ImageGray =
  lift1(g => Grayscale(1.0 - g))

def grayToColor: ImageGray => ImageColor = invert.andThen(lift1(Color.gray))

def maskToColor: ImageMask => ImageColor = maskToGray.andThen(grayToColor)

def filterIm[A](pred: ImageMask)(using pv: PixelValue[A]): ImageT[A] =
  im => select(pred)(im)(lift0(pv.white))

def fromPolarIm[A](im: PolarImage[A]): Image[A] =
  p => im(p.toPolar())

def toPolarIm[A](im: Image[A]): PolarImage[A] =
  p => im(p.toPoint())

def transformCoord[A](f: Point => Point): ImageT[A] =
  im => p => im(f(p))

def flipV[A]: ImageT[A] =
  transformCoord { case Point(x, y) => Point(x, -y) }

def stretchX[A](k: Double): ImageT[A] =
  transformCoord { case Point(x, y) => Point(x / k, y) }

def rotateIm[A](θ: Double): ImageT[A] =
  transformCoord(Point.rotate(-θ))

def swirlIm[A](θ: Double): ImageT[A] =
  transformCoord(Point.swirl(-θ))

def scaleIm[A](k: Double): ImageT[A] =
  transformCoord(Point.scale(1 / k))

def scaleIm2[A](kX: Double, kY: Double): ImageT[A] =
  transformCoord(Point.scale2(1 / kX, 1 / kY))

def translateIm[A](v: Vector): ImageT[A] =
  transformCoord(Point.translate(Vector(-v.x, -v.y)))

// Mask Algebra
val fullM: ImageMask = lift0(true) // selects all points
val emptyM: ImageMask = lift0(false) // selects no points

// Negate a mask
def notM(mask: ImageMask): ImageMask =
  p => !mask(p)

// Intersection of two masks
def intersectM(a: ImageMask, b: ImageMask): ImageMask =
  lift2[Boolean, Boolean, Boolean](_ && _)(a)(b)

// Union of two masks
def unionM(a: ImageMask, b: ImageMask): ImageMask =
  lift2[Boolean, Boolean, Boolean](_ || _)(a)(b)

// Exclusive OR of two masks
def xorM(a: ImageMask, b: ImageMask): ImageMask =
  lift2[Boolean, Boolean, Boolean]((x, y) => (x || y) && !(x && y))(a)(b)

// Horizontal juxtaposition
def horizontalJuxtapose[A](f: Image[A])(g: Image[A]): Image[A] =
  p => {
    val Point(x, y) = p
    val img1 = f(Point(2 * x + 1, y))
    val img2 = g(Point(2 * x - 1, y))
    if (x < 0) img1 else img2
  }

// Vertical juxtaposition
def verticalJuxtapose[A](f: Image[A])(g: Image[A]): Image[A] =
  rotateIm(-math.Pi / 2)(
    horizontalJuxtapose(rotateIm(math.Pi / 2)(f))(rotateIm(math.Pi / 2)(g))
  )

// General horizontal juxtaposition for a list of images
def juxtapose[A](images: List[Image[A]]): Image[A] = { p =>
  val Point(x, y) = p
  val n = images.length
  val imageNum = (x + 1) * n / 2.0
  val index =
    math.floor(if imageNum.isInfinity then -imageNum else imageNum).toInt
  val normalIndex = if (index < 0) 0 else if (index >= n) n - 1 else index
  val fracX = imageNum - index
  val image = images(normalIndex)
  val newX = 2 * fracX - 1
  val mehmet = image(Point(newX, y))
  // println(s"$p, $index, $normalIndex, $newX ,$mehmet")
  mehmet
}
