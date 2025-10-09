package bismuth

case class Color(r: Double, g: Double, b: Double, a: Double) {
  // Interpolate
  def interpolate(that: Color, w: Double): Color = {
    def mix(v1: Double, v2: Double): Double = (1 - w) * v1 + w * v2

    Color(
      mix(this.r, that.r),
      mix(this.g, that.g),
      mix(this.b, that.b),
      mix(this.a, that.a)
    )
  }
  // Complement
  def complement(): Color =
    Color(1 - r, 1 - g, 1 - b, a)

  // Set Opacity
  def setOpacity(α: Double): Color = this.copy(a = α)

  // -
  def -(that: Color): Color = {
    Color(
      Math.max(0.0, r - that.r),
      Math.max(0.0, b - that.b),
      Math.max(0.0, g - that.g),
      a
    )
  }
}

object Color {
  def gray(value: Double): Color = Color(value, value, value, 1.0)

  val red: Color = Color(1, 0, 0, 1)
  val green: Color = Color(0, 1, 0, 1)
  val blue: Color = Color(0, 0, 1, 1)
  val yellow: Color = blue.complement()
  val black: Color = gray(0)
  val white: Color = Color(1, 1, 1, 1)

  // Grayscale
  opaque type Grayscale = Double
  object Grayscale {
    def apply(value: Double): Grayscale = value
  }
  def injRed(g: Grayscale): Color = Color(g, 0, 0, 1)
  def injGreen(g: Grayscale): Color = Color(0, g, 0, 1)
  def injBlue(g: Grayscale): Color = Color(0, 0, g, 1)
  def injBlack(g: Grayscale): Color = white - Color(g, g, g, 1)
}
