import scala.util.Random
case class Point(val x: Double, val y: Double) extends Ordered[Point] {
  def compare(that: Point): Int = {
    if (this.x != that.x) this.x.compare(that.x)
    else this.y.compare(that.y)
  }

  /*Renvoie vrai si self est dans le cercle circonscrit de a, b, et c*/
  def is_incircle(a: Point, b: Point, c: Point): Boolean = {
    assume(!Point.areCollinear(a, b, c), "les point sont colinéaire")
    assume(a != b, s"a et b sont pareil (i.e = $a)")
    assume(b != c, s"b et c sont pareil (i.e = $b)")
    assume(a != c, s"a et c sont pareil (i.e = $c)")
    if (a == this || b == this || c == this) {
      true
    }

    def squaredDistance(p1: Point, p2: Point): Double = {
      val dx = p1.x - p2.x
      val dy = p1.y - p2.y
      dx * dx + dy * dy
    }

    val centre = Point.circumcenter(a, b, c)

    val radiusSquared = squaredDistance(centre, a)
    val distanceToSelfSquared = squaredDistance(centre, this)

    // <= car == si sur le cerle
    distanceToSelfSquared <= radiusSquared
  }

  // cross product (0 == colineaire, 1==CCW, -1==CW)
  def ^(rhs: Point): Double = this.x * rhs.y - this.y * rhs.x

  def -(rhs: Point): Point = Point(this.x - rhs.x, this.y - rhs.y)

}

object Point {
  def areCollinear(a: Point, b: Point, c: Point) = {
    assume(a != b, s"a et b sont pareil (i.e = $a)")
    assume(b != c, s"b et c sont pareil (i.e = $b)")
    assume(a != c, s"a et c sont pareil (i.e = $c)")
    val A = a - c
    val B = b - c
    // not compare psq on veut 0.0 == -0.0
    (A ^ B) == 0.0
  }

  /** true si counterclockwise-oriented triangle ie si b et faux pour c - a / \
    * \ / c - a b cross > 0
    */
  def ccw(a: Point, b: Point, c: Point): Boolean = {
    assume(a != b, s"a et b sont pareil (i.e = $a)")
    assume(b != c, s"b et c sont pareil (i.e = $b)")
    assume(a != c, s"a et c sont pareil (i.e = $c)")
    val A = b - a
    val B = c - a
    (A ^ B) > 0.0
  }

  /** d in ABC */
  def incircle(pa: Point, pb: Point, pc: Point, pd: Point): Boolean = {
    pd.is_incircle(pa, pb, pc)
  }

  def circumcenter(a: Point, b: Point, c: Point): Point = {
    assume(!Point.areCollinear(a, b, c), "les point sont colinéaire")
    assume(a != b, s"a et b sont pareil (i.e = $a)")
    assume(b != c, s"b et c sont pareil (i.e = $b)")
    assume(a != c, s"a et c sont pareil (i.e = $c)")

    def norme2(p: Point): Double = {
      p.x * p.x + p.y * p.y
    }

    val (an, bn, cn) = (norme2(a), norme2(b), norme2(c))

    // Formule de cramer
    //     det(A_x)         a(b.y-c.y)+b(c.y-a.y)+c(a.y-b.y)
    // x = ---------- = -------------------------------------
    //      det(A)       2xa(b.y-c.y)+2xb(c.y-a.y)+2xc(a.y-b.y)
    // où det(A) est l'equation catésienne de cercle circonscrit

    // Denominator
    val D = 2 * (a.x * (b.y - c.y) + b.x * (c.y - a.y) + c.x * (a.y - b.y));
    val Ux = (an * (b.y - c.y) + bn * (c.y - a.y) + cn * (a.y - b.y)) / D;
    val Uy = (an * (c.x - b.x) + bn * (a.x - c.x) + cn * (b.x - a.x)) / D;
    Point(Ux, Uy)
  }

  val Infinity: Point = Point(Double.PositiveInfinity, Double.PositiveInfinity)

  def centroid(points: Seq[Point]): Point = {
    val validPoints = points.filterNot(p => p == Infinity)
    if (validPoints.nonEmpty) {
      val xs = validPoints.map(_.x)
      val ys = validPoints.map(_.y)
      Point(xs.sum / xs.size, ys.sum / ys.size)
    } else {
      Infinity
    }
  }

  def generatePoints(numPoints: Int): Set[Point] =
    generatePoints(numPoints, (0, 1), (0, 1))
  def generatePoints(
      numPoints: Int,
      xRange: (Double, Double),
      yRange: (Double, Double)
  ): Set[Point] = {
    val rand = new Random()

    (1 to numPoints).map { _ =>
      val x = xRange._1 + (xRange._2 - xRange._1) * rand.nextDouble()
      val y = yRange._1 + (yRange._2 - yRange._1) * rand.nextDouble()
      Point(x, y)
    }.toSet
  }

}
