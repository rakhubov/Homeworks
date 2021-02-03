sealed trait Moveable {
  def move(dx: Double, dy: Double): Moveable
}

sealed trait Located {
  def x: Double
  def y: Double
}

sealed trait Bounded {
  def minX: Double
  def maxX: Double
  def minY: Double
  def maxY: Double
}

sealed trait Shape extends Located with Bounded with Moveable

final case class Point(x: Double, y: Double) extends Shape {
  override def minX: Double = x
  override def maxX: Double = x
  override def minY: Double = y
  override def maxY: Double = y
  override def move(dx: Double, dy: Double): Point = Point(x + dx, y + dy)
}

final case class Circle(centerX: Double, centerY: Double, radius: Double)
    extends Shape {
  override def x: Double = centerX
  override def y: Double = centerY
  override def minX: Double = centerX - radius
  override def maxX: Double = centerX + radius
  override def minY: Double = centerY - radius
  override def maxY: Double = centerY + radius
  override def move(dx: Double, dy: Double): Circle =
    Circle(centerX + dx, centerY + dy, radius)
}

final case class Rectangle(
    centerX: Double,
    centerY: Double,
    width: Double,
    height: Double
) extends Shape {
  override def x: Double = centerX
  override def y: Double = centerY
  override def minX: Double = centerX - width / 2
  override def maxX: Double = centerX + width / 2
  override def minY: Double = centerY - height / 2
  override def maxY: Double = centerY + height / 2
  override def move(dx: Double, dy: Double): Rectangle =
    Rectangle(centerX + dx, centerY + dy, width, height)
}
