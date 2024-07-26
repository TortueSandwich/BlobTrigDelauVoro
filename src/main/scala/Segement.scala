case class Segment(A: Point, B: Point) {
  def middle: Point = Point((A.x + B.x) / 2, (A.y + B.y) / 2)

  def minX: Double = Math.min(A.x, B.x)

  def maxX: Double = Math.max(A.x, B.x)

  def minY: Double = Math.min(A.y, B.y)

  def maxY: Double = Math.max(A.y, B.y)

  def directionVector: Point = Point(B.x - A.x, B.y - A.y)

  // ax + by + c = 0
  def cartesianEquation: (Double, Double, Double) = {
    val Point(dx, dy) = directionVector
    val a = dy
    val b = -dx
    val c = dx * A.y - dy * A.x
    (a, b, c)
  }

  def slopeIntercept: (Double, Double) = {
    val m = (B.y - A.y) / (B.x - A.x)
    (m, A.y - m * A.x)
  }

  def isPointOnLine(p: Point): Boolean = {
    val (m, b) = this.slopeIntercept
    m * p.x + b == p.y
  }

  def isPointOnSegment(p: Point): Boolean = {
    isPointOnLine(p) && minX <= p.x && p.x <= maxX && minY <= p.y && p.y <= maxY
  }

  // ax + by + c = 0 for perpendicular bisector
  def perpendicularBisector: (Double, Double, Double) = {
    val mid = middle
    val Point(dx, dy) = directionVector
    val a = -dy
    val b = dx
    val c = -a * mid.x - b * mid.y
    (a, b, c)
  }

  def length: Double = Point.euclidian(A, B)

  // Symmetric point with respect to the segment line
  def symmetricPoint(p: Point): Point = {
    println("---")
    val (a, b, c) = cartesianEquation
    val d = (a * p.x + b * p.y + c) / (a * a + b * b)
    print(a, b, c, d)
    val foot = Point(p.x - a * d, p.y - b * d)
    println("---")
    Point(2 * foot.x - p.x, 2 * foot.y - p.y)
  }
}
