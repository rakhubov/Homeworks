object figures extends App {

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
  //
  //
  //
  // Implement HomeWork 2
  //
  //
  //
  //

  sealed trait Area {
    def area: Double
  }

  def side_length(p1: Point, p2: Point): Double = {
    math.sqrt(math.pow((p1.x - p2.x), 2) + math.pow((p1.y - p2.y), 2))
  }

  def side_length3D(p1: Point3D, p2: Point3D): Double = {
    math.sqrt(
      math.pow((p1.x - p2.x), 2) + math.pow((p1.y - p2.y), 2) + math
        .pow((p1.z - p2.z), 2)
    )
  }

  final case class Triangle(point1: Point, point2: Point, point3: Point)
      extends Shape
      with Area {
    val setpoint = Set(point1, point2, point3)
    override def x: Double = (point1.x + point2.x + point3.x) / 3
    override def y: Double = (point1.y + point2.y + point3.y) / 3
    override def minX: Double = setpoint.map(_.minX).min
    override def maxX: Double = setpoint.map(_.maxX).max
    override def minY: Double = setpoint.map(_.minY).min
    override def maxY: Double = setpoint.map(_.maxY).max

    override def move(dx: Double, dy: Double): Triangle =
      Triangle(
        point1.copy(x + dx, y + dy),
        point2.copy(x + dx, y + dy),
        point3.copy(x + dx, y + dy)
      )

    val sl1 = side_length(point1, point2)
    val sl2 = side_length(point2, point3)
    val sl3 = side_length(point3, point1)

    override def area: Double =
      math.sqrt(
        ((sl1 + sl2 + sl3) / 2) * ((sl1 + sl2 + sl3) / 2 - sl1) * ((sl1 + sl2 + sl3) / 2 - sl2) * ((sl1 + sl2 + sl3) / 2 - sl3)
      )
  }

  final case class Square(
      point1: Point,
      point2: Point,
      point3: Point,
      point4: Point
  ) extends Shape
      with Area {
    val setpoint = Set(point1, point2, point3, point4)
    override def x: Double = (point1.x + point2.x + point3.x + point4.x) / 4
    override def y: Double = (point1.y + point2.y + point3.y + point4.y) / 4
    override def minX: Double = setpoint.map(_.minX).min
    override def maxX: Double = setpoint.map(_.maxX).max
    override def minY: Double = setpoint.map(_.minY).min
    override def maxY: Double = setpoint.map(_.maxY).max

    override def move(dx: Double, dy: Double): Square =
      Square(
        point1.copy(x + dx, y + dy),
        point2.copy(x + dx, y + dy),
        point3.copy(x + dx, y + dy),
        point4.copy(x + dx, y + dy)
      )
    val sl1 = side_length(point1, point2)
    override def area: Double = sl1 * sl1
  }
  //
  //
  //
  sealed trait Located3D {
    def z: Double
  }
  sealed trait Bounded3D {
    def minZ: Double
    def maxZ: Double
  }
  sealed trait Moveable3D {
    def move(dx: Double, dy: Double, dz: Double): Moveable3D
  }
  sealed trait morefunk {
    def surfaseArea: Double
    def volume: Double
  }

  sealed trait Shape3D
      extends Located
      with Bounded
      with Moveable3D
      with Located3D
      with Bounded3D

  final case class Point3D(x: Double, y: Double, z: Double) extends Shape3D {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def minZ: Double = z
    override def maxZ: Double = z
    override def move(dx: Double, dy: Double, dz: Double): Point3D =
      Point3D(x + dx, y + dy, z + dz)
  }

  final case class Triangle3D(point1: Point3D, point2: Point3D, point3: Point3D)
      extends Shape3D
      with Area
      with morefunk {
    val setpoint = Set(point1, point2, point3)
    override def x: Double = (point1.x + point2.x + point3.x) / 3
    override def y: Double = (point1.y + point2.y + point3.y) / 3
    override def z: Double = (point1.z + point2.z + point3.z) / 3
    override def minX: Double = setpoint.map(_.minX).min
    override def maxX: Double = setpoint.map(_.maxX).max
    override def minY: Double = setpoint.map(_.minY).min
    override def maxY: Double = setpoint.map(_.maxY).max
    override def minZ: Double = setpoint.map(_.minZ).min
    override def maxZ: Double = setpoint.map(_.maxZ).max
    override def move(dx: Double, dy: Double, dz: Double): Triangle3D =
      Triangle3D(
        point1.copy(x + dx, y + dy, z + dz),
        point2.copy(x + dx, y + dy, z + dz),
        point3.copy(x + dx, y + dy, z + dz)
      )
    val sl1 = side_length3D(point1, point2)
    val sl2 = side_length3D(point2, point3)
    val sl3 = side_length3D(point3, point1)
    override def area: Double =
      math.sqrt(
        ((sl1 + sl2 + sl3) / 2) * ((sl1 + sl2 + sl3) / 2 - sl1) * ((sl1 + sl2 + sl3) / 2 - sl2) * ((sl1 + sl2 + sl3) / 2 - sl3)
      )
    def surfaseArea: Double = area
    def volume: Double = 0
  }

  final case class Cube(
      point1: Point3D,
      point2: Point3D,
      point3: Point3D,
      point4: Point3D,
      point5: Point3D,
      point6: Point3D,
      point7: Point3D,
      point8: Point3D
  ) extends Shape3D
      with morefunk {
    val setpoint =
      Set(point1, point2, point3, point4, point5, point6, point7, point8)
    override def x: Double =
      (point1.x + point2.x + point3.x + point4.x + point5.x + point6.x + point7.x + point8.x) / 8
    override def y: Double =
      (point1.y + point2.y + point3.y + point4.y + point5.y + point6.y + point7.y + point8.y) / 8
    override def z: Double =
      (point1.z + point2.z + point3.z + point4.z + point5.z + point6.z + point7.z + point8.z) / 8
    override def minX: Double = setpoint.map(_.minX).min
    override def maxX: Double = setpoint.map(_.maxX).max
    override def minY: Double = setpoint.map(_.minY).min
    override def maxY: Double = setpoint.map(_.maxY).max
    override def minZ: Double = setpoint.map(_.minZ).min
    override def maxZ: Double = setpoint.map(_.maxZ).max

    override def move(dx: Double, dy: Double, dz: Double): Cube =
      Cube(
        point1.copy(x + dx, y + dy, z + dz),
        point2.copy(x + dx, y + dy, z + dz),
        point3.copy(x + dx, y + dy, z + dz),
        point4.copy(x + dx, y + dy, z + dz),
        point5.copy(x + dx, y + dy, z + dz),
        point6.copy(x + dx, y + dy, z + dz),
        point7.copy(x + dx, y + dy, z + dz),
        point8.copy(x + dx, y + dy, z + dz)
      )
    val sl1 = side_length3D(point1, point2)
    override def surfaseArea: Double = sl1 * sl1 * 6
    override def volume: Double = sl1 * sl1 * sl1
  }

  final case class Sphere(point1: Point3D, radius: Double)
      extends Shape3D
      with morefunk {
    override def x: Double = point1.x
    override def y: Double = point1.y
    override def z: Double = point1.z
    override def minX: Double = point1.x - radius
    override def maxX: Double = point1.x + radius
    override def minY: Double = point1.y - radius
    override def maxY: Double = point1.y + radius
    override def minZ: Double = point1.z - radius
    override def maxZ: Double = point1.z + radius
    override def move(dx: Double, dy: Double, dz: Double): Sphere =
      Sphere(point1.copy(x + dx, y + dy, z + dz), radius)
    override def surfaseArea: Double = 4 * math.Pi * radius * radius
    override def volume: Double = 4 / 3 * math.Pi * radius * radius * radius
  }

}
