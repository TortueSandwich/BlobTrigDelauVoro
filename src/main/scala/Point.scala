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
    val centre = Point.circumcenter(a, b, c)

    val radiusDistance = Point.squaredEuclidian(centre, a)
    val distanceToSelf = Point.squaredEuclidian(centre, this)

    // <= car == si sur le cerle
    distanceToSelf <= radiusDistance
  }

  // cross product (=0:colineaire, >0:CCW, <0:CW)
  def ^(rhs: Point): Double = this.x * rhs.y - this.y * rhs.x

  def +(rhs: Point): Point = Point(this.x + rhs.x, this.y + rhs.y)
  def -(rhs: Point): Point = Point(this.x - rhs.x, this.y - rhs.y)

  def isPointInTriangle(a: Point, b: Point, c: Point): Boolean = {
    // Utilisation du produit vectoriel pour vérifier le signe de la zone de chaque triangle formé avec le point p
    def sign(p1: Point, p2: Point, p3: Point): Double = {
      (p1.x - p3.x) * (p2.y - p3.y) - (p2.x - p3.x) * (p1.y - p3.y)
    }

    // Calcul des signes des produits vectoriels
    val d1 = sign(this, a, b)
    val d2 = sign(this, b, c)
    val d3 = sign(this, c, a)

    // Vérifie si les signes sont tous positifs ou tous négatifs, ce qui signifie que le point est dans le triangle
    val hasNeg = (d1 < 0) || (d2 < 0) || (d3 < 0)
    val hasPos = (d1 > 0) || (d2 > 0) || (d3 > 0)

    !(hasNeg && hasPos)
  }

  def scale(a: Double): Point = Point(this.x * a, this.y * a)
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

  /** true si counterclockwise-oriented triangle ie
    * ```txt
    * si   b    et faux pour c  -  a
    *    /   \                \  /
    *   c  -  a                b
    * ```
    *
    * cross > 0
    */
  def ccw(a: Point, b: Point, c: Point): Boolean = {
    assume(a != b, s"a et b sont pareil (i.e = $a)")
    assume(b != c, s"b et c sont pareil (i.e = $b)")
    assume(a != c, s"a et c sont pareil (i.e = $c)")
    val A = b - a
    val B = c - a
    (A ^ B) > 0.0
  }

  def circumcenter(a: Point, b: Point, c: Point): Point = {
    assume(!Point.areCollinear(a, b, c), "les point sont colinéaire")
    assume(a != b, s"a et b sont pareil (i.e = $a)")
    assume(b != c, s"b et c sont pareil (i.e = $b)")
    assume(a != c, s"a et c sont pareil (i.e = $c)")

    def N(p: Point): Double = squaredEuclidian(p, Point.Origin)

    val (an, bn, cn) = (N(a), N(b), N(c))

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
  val Origin: Point = Point(0, 0)

  implicit def squaredEuclidian(a: Point, b: Point): Double =
    Math.pow(a.x - b.x, 2) + Math.pow(a.y - b.y, 2)
  def euclidian(a: Point, b: Point): Double =
    Math.sqrt(Math.pow(a.x - b.x, 2) + Math.pow(a.y - b.y, 2))

  def manathan(a: Point, b: Point): Double = (a.x - b.x).abs + (a.y - b.y).abs
  def sup(a: Point, b: Point): Double =
    Math.max((a.x - b.x).abs, (a.y - b.y).abs)

  def sncf(a: Point, b: Point): Double =
    if (a == b) 0.0
    else if (
      a != Point.Origin && b != Point.Origin && Point.areCollinear(
        a,
        b,
        Point.Origin
      )
    ) euclidian(a - b, Point.Origin)
    else euclidian(a, Point.Origin) + euclidian(b, Point.Origin)

  def camberra(a: Point, b: Point): Double =
    (a.x - b.x).abs / (a.x.abs + b.x.abs) + (a.y - b.y).abs / (a.y.abs + b.y.abs)

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
