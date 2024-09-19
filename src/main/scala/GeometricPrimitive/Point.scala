package GeometricPrimitives

import scala.util.Random

sealed trait Point

case object InfinitePoint extends Point

object CounterClockwiseComparator {
  def apply(center: FinitePoint): Ordering[FinitePoint] = new Ordering[FinitePoint] {
    def compare(p1: FinitePoint, p2: FinitePoint): Int = {
      val angle1 = math.atan2(p1.y - center.y, p1.x - center.x)
      val angle2 = math.atan2(p2.y - center.y, p2.x - center.x)
      angle1.compareTo(angle2)
    }
  }
}

case class FinitePoint(x: Double, y: Double) extends Point with Ordered[FinitePoint] {
  def compare(that: FinitePoint): Int = {
    if (this.x != that.x) this.x.compare(that.x)
    else this.y.compare(that.y)
  }

  def is_incircle(a: FinitePoint, b: FinitePoint, c: FinitePoint): Boolean = {
    assume(!FinitePoint.areCollinear(a, b, c), "les points sont colinéaires")
    assume(a != b, s"a et b sont pareils (i.e = $a)")
    assume(b != c, s"b et c sont pareils (i.e = $b)")
    assume(a != c, s"a et c sont pareils (i.e = $c)")

    if (a == this || b == this || c == this) {
      true
    }
    val centre = FinitePoint.circumcenter(a, b, c)

    val radiusDistance = FinitePoint.squaredEuclidian(centre, a)
    val distanceToSelf = FinitePoint.squaredEuclidian(centre, this)

    distanceToSelf <= radiusDistance
  }

  def ^(rhs: FinitePoint): Double = this.x * rhs.y - this.y * rhs.x

  def +(rhs: FinitePoint): FinitePoint = FinitePoint(this.x + rhs.x, this.y + rhs.y)
  def -(): FinitePoint = FinitePoint(-this.x, -this.y)
  def -(rhs: FinitePoint): FinitePoint = FinitePoint(this.x - rhs.x, this.y - rhs.y)

  def *(rhs : Double): FinitePoint = FinitePoint(this.x*rhs, this.y*rhs)

  def isPointInTriangle(a: FinitePoint, b: FinitePoint, c: FinitePoint): Boolean = {
    def sign(p1: FinitePoint, p2: FinitePoint, p3: FinitePoint): Double = {
      (p1.x - p3.x) * (p2.y - p3.y) - (p2.x - p3.x) * (p1.y - p3.y)
    }

    val d1 = sign(this, a, b)
    val d2 = sign(this, b, c)
    val d3 = sign(this, c, a)

    val hasNeg = (d1 < 0) || (d2 < 0) || (d3 < 0)
    val hasPos = (d1 > 0) || (d2 > 0) || (d3 > 0)

    !(hasNeg && hasPos)
  }

  def scale(a: Double): FinitePoint = FinitePoint(this.x * a, this.y * a)

  // not automatic ????
  def unapply(): (Double, Double) = (x, y)
}


object FinitePoint {
  
  // i struggle to use it ????
  implicit def pointToTuple(p: FinitePoint): (Double, Double) = (p.x, p.y)
  

  def areCollinear(a: FinitePoint, b: FinitePoint, c: FinitePoint): Boolean = {
    assume(a != b, s"a et b sont pareils (i.e = $a)")
    assume(b != c, s"b et c sont pareils (i.e = $b)")
    assume(a != c, s"a et c sont pareils (i.e = $c)")
    val A = a - c
    val B = b - c
    (A ^ B) == 0.0
  }

  def ccw(a: FinitePoint, b: FinitePoint, c: FinitePoint): Boolean = {
    assume(a != b, s"a et b sont pareils (i.e = $a)")
    assume(b != c, s"b et c sont pareils (i.e = $b)")
    assume(a != c, s"a et c sont pareils (i.e = $c)")
    val A = b - a
    val B = c - a
    (A ^ B) > 0.0
  }

  def circumcenter(a: FinitePoint, b: FinitePoint, c: FinitePoint): FinitePoint = {
    assume(!areCollinear(a, b, c), "les points sont colinéaires")
    assume(a != b, s"a et b sont pareils (i.e = $a)")
    assume(b != c, s"b et c sont pareils (i.e = $b)")
    assume(a != c, s"a et c sont pareils (i.e = $c)")

    def N(p: FinitePoint): Double = squaredEuclidian(p, FinitePoint.Origin)

    val (an, bn, cn) = (N(a), N(b), N(c))
    val D = 2 * (a.x * (b.y - c.y) + b.x * (c.y - a.y) + c.x * (a.y - b.y))
    val Ux = (an * (b.y - c.y) + bn * (c.y - a.y) + cn * (a.y - b.y)) / D
    val Uy = (an * (c.x - b.x) + bn * (a.x - c.x) + cn * (b.x - a.x)) / D
    FinitePoint(Ux, Uy)
  }

  val Origin: FinitePoint = FinitePoint(0, 0)

  implicit def squaredEuclidian(a: FinitePoint, b: FinitePoint): Double =
    Math.pow(a.x - b.x, 2) + Math.pow(a.y - b.y, 2)

  def euclidian(a: FinitePoint, b: FinitePoint): Double =
    Math.sqrt(Math.pow(a.x - b.x, 2) + Math.pow(a.y - b.y, 2))

  def centroid(points: Seq[FinitePoint]): FinitePoint = {
    val xs = points.map(_.x)
    val ys = points.map(_.y)
    FinitePoint(xs.sum / xs.size, ys.sum / ys.size)
  }

  def generatePoints(numPoints: Int): Set[FinitePoint] =
    generatePoints(numPoints, (0, 1), (0, 1))

  def generatePoints(
      numPoints: Int,
      xRange: (Double, Double),
      yRange: (Double, Double)
  ): Set[FinitePoint] = {
    val rand = new Random()

    (1 to numPoints).map { _ =>
      val x = xRange._1 + (xRange._2 - xRange._1) * rand.nextDouble()
      val y = yRange._1 + (yRange._2 - yRange._1) * rand.nextDouble()
      FinitePoint(x, y)
    }.toSet
  }

  /** centre cercle incrit (toujours dans le triangle)*/
  def incenter(a: FinitePoint,b : FinitePoint,c: FinitePoint) : FinitePoint = {
    assume(!areCollinear(a, b, c), "les points sont colinéaires")
    assume(a != b, s"a et b sont pareils (i.e = $a)")
    assume(b != c, s"b et c sont pareils (i.e = $b)")
    assume(a != c, s"a et c sont pareils (i.e = $c)")
    val na: Double = euclidian(b, c)
    val nb: Double = euclidian(a, c)
    val nc: Double = euclidian(a, b)
    
    val p = na + nb + nc
    
    val I_x = (na * a.x + nb * b.x + nc * c.x) / p
    val I_y = (na * a.y + nb * b.y + nc * c.y) / p

    FinitePoint(I_x, I_y)
  }

  def getLocalMinDist(pts: Iterable[FinitePoint], x : FinitePoint) : Double = {
      pts.foldLeft(Double.PositiveInfinity)(
        ((acc, y) => if (x==y) acc else Math.min(acc, FinitePoint.euclidian(x, y)) )
      )
    }

  def getAverageMinDist(pts: Iterable[FinitePoint]): Double = {
    val points = pts.toSeq
    val minDistance = points.foldLeft(0.0)((acc, p) => acc + FinitePoint.getLocalMinDist(points, p))
    minDistance/pts.size
  }

  def getMaxDist(pts: Iterable[FinitePoint]) = {
    Math.sqrt(2/(Math.sqrt(3)*pts.size))
  }
}
