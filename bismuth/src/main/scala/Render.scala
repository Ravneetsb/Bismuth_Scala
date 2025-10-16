package bismuth

import java.awt.image.BufferedImage
import java.awt.Color as JavaColor
import scala.reflect.ClassTag

// Flip an image vertically
def flipVRender[A](im: Image[A]): Image[A] =
  p => im(Point(p.x, -p.y))

def render[A](im: Image[A], width: Int, height: Int)(using
    pv: PixelValue[A]
): BufferedImage = {
  val img = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)

  def mapCoord(a: Int, b: Int): Double =
    (2.0 * a + 1.0) / b - 1.0

  for y <- 0 until height do
    for x <- 0 until width do
      val px = mapCoord(x, width)
      val py = mapCoord(y, height)
      // println(s"$px, $py")
      val color: JavaColor = pv.toPNGPixel(flipV(im)(Point(px, py)))
      img.setRGB(x, y, color.getRGB)

  img
}

def renderToArray[A: ClassTag](
    im: Image[A],
    width: Int,
    height: Int
): Array[Array[A]] = {

  def mapOverCoord(f: Double => A, n: Int): Array[A] =
    Array.tabulate(n)(i => f(i.toDouble / n))

  Array.tabulate(height) { y =>
    mapOverCoord(x => im(Point(x, y.toDouble / height)), width)
  }
}
