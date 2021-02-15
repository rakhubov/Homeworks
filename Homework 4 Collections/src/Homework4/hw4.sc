

def maxWidthOfVerticalArea(points: Array[Array[Int]]): Int = {
  val x = points.map(_.head).sorted
  x.zip(x.tail).flatMap { case (a, b) => Array(b - a) }.max
}



val a = Array ((1,25),(1,2))
//val s = Array(a.map(_.head))
val v = List(1,2,3,4)

v.find(_ == 0)






