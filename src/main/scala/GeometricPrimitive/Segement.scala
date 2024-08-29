case class Segment(A: FinitePoint, B: FinitePoint) {
  def middle: FinitePoint = FinitePoint((A.x + B.x) / 2, (A.y + B.y) / 2)

  def minX: Double = Math.min(A.x, B.x)

  def maxX: Double = Math.max(A.x, B.x)

  def minY: Double = Math.min(A.y, B.y)

  def maxY: Double = Math.max(A.y, B.y)

  def directionVector: FinitePoint = FinitePoint(B.x - A.x, B.y - A.y)

  /** ax + by + c = 0 */
  def cartesianEquation: (Double, Double, Double) = {
    val FinitePoint(dx, dy) = directionVector
    val a = dy
    val b = -dx
    val c = dx * A.y - dy * A.x
    (a, b, c)
  }

  /** y = mx+b */
  def slopeIntercept: (Double, Double) = {
    val m = (B.y - A.y) / (B.x - A.x)
    (m, A.y - m * A.x)
  }

  def isPointOnLine(p: FinitePoint): Boolean = {
    val (m, b) = this.slopeIntercept
    m * p.x + b == p.y
  }

  def isPointOnSegment(p: FinitePoint): Boolean = {
    isPointOnLine(p) && minX <= p.x && p.x <= maxX && minY <= p.y && p.y <= maxY
  }

  // ax + by + c = 0 for perpendicular bisector
  def perpendicularBisector: (Double, Double, Double) = {
    val mid = middle
    val FinitePoint(dx, dy) = directionVector
    val a = -dy
    val b = dx
    val c = -a * mid.x - b * mid.y
    (a, b, c)
  }

  /** euclidian */
  def length: Double = FinitePoint.euclidian(A, B)

  // Symmetric point with respect to the segment line
  def symmetricPoint(p: FinitePoint): FinitePoint = {
    println("---")
    val (a, b, c) = cartesianEquation
    val d = (a * p.x + b * p.y + c) / (a * a + b * b)
    print(a, b, c, d)
    val foot = FinitePoint(p.x - a * d, p.y - b * d)
    println("---")
    FinitePoint(2 * foot.x - p.x, 2 * foot.y - p.y)
  }

  def normalizedDirection: FinitePoint = {
    val vec = directionVector
    val norm = math.sqrt(vec.x * vec.x + vec.y * vec.y)
    FinitePoint(vec.x / norm, vec.y / norm)
  }

  def intersection(s : Segment): Option[(Double, Double)] = {
    val Segment(p1, p2) = this
    val Segment(p3, p4) = s
    val denom = (p1.x-p2.x)*(p3.y-p4.y)-(p1.y -p2.y)*(p3.x-p4.x)
    denom match {
      case 0.0 | -0.0 => None
      case denom =>
        val x = (p1.x*p2.y - p1.y*p2.x)*(p3.x-p4.x) - (p1.x-p2.x)*(p3.x*p4.y-p3.y*p4.x)
        val y = (p1.x*p2.y - p1.y*p2.x)*(p3.y-p4.y) - (p1.y-p2.y)*(p3.x*p4.y-p3.y*p4.x)
        Some((x/denom, y/denom))
    }
  }
}
