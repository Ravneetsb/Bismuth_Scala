package bismuth

case class Point(x: Double, y: Double) {

  def norm(): Double = Math.sqrt((x * x) + (y * y))

  // Convert to polar point
  def toPolar(): Polar = {
    val r = this.norm()
    val θ =
      if (r == 0) {
        0.0
      } else {
        val angle = Math.acos(x / r)
        if (y < 0) (2 * Math.PI) - angle else angle
      }
    return Polar(r, θ)
  }

  // Midpoint
  def midpoint(other: Point): Point =
    Point((x + other.x) / 2, (y + other.y) / 2)

  // Rotation

  // Swirling
}

object Point {
  def rotate(θ: Double)(p: Point): Point = {
    val c = Math.cos(θ)
    val s = Math.sin(θ)
    return Point(c * p.x - s * p.y, s * p.x + c * p.y)
  }
  // Translation
  def translate(v: Vector)(p: Point): Point = Point(p.x + v.x, p.y + v.y)

  // Scaling
  def scale(k: Double)(p: Point): Point = Point(p.x * k, p.y * k)

  def scale2(kx: Double, ky: Double)(p: Point): Point =
    Point(kx * p.x, ky * p.y)
  def swirl(θ: Double)(p: Point): Point = rotate(θ * p.norm())(p)
}

case class Polar(r: Double, θ: Double) {
  def toPoint(): Point = {
    val x = r * Math.cos(θ)
    val y = r * Math.sin(θ)
    return Point(x, y)
  }

  def midpoint(other: Polar): Polar = {
    this.toPoint().midpoint(other.toPoint()).toPolar()
  }
}

case class Vector(x: Double, y: Double) {}
