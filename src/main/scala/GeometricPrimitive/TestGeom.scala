object TestGeom extends App {

  test_colineaire()
  test_CCW()
  // test_incircle()
  println("\u001b[92m\u001b[1mToute les assertions sont passées\u001b[0m")

  def test_CCW() {
    val O = Point(0.0, 0.0)
    val A = Point(0.0, 1.0)
    val B = Point(1.0, 0.0)
    val C = Point(0.0, -1.0)
    assert(!Point.ccw(O, A, B), "OAB")
    assert(!Point.ccw(A, B, O), "ABO")
    assert(!Point.ccw(B, O, A), "BOA")
    assert(Point.ccw(O, B, A), "OBA")
    assert(!Point.ccw(O, A, C), "OAC")
    assert(!Point.ccw(O, B, C), "OBC")

    assert(!Point.ccw(Point(1, 0), Point(-1, 0), Point(-2, 2)), "Test unitaire")

    println("test test_CCW() ok")
  }

  def test_colineaire() {
    val A = Point(0.0, 0.0)
    val B = Point(0.0, 2.0)
    val C = Point(0.0, 5.0)
    val D = Point(1.0, 0.0)
    assert(Point.areCollinear(A, B, C), "c'est colineaire1")
    assert(Point.areCollinear(A, C, B), "c'est colineaire2")
    assert(Point.areCollinear(C, A, B), "c'est colineaire3")
    assert(!Point.areCollinear(A, B, D), "ce n'dest pas colinire")
    println("test test_colineaire() ok")
  }

  def test_incircle() {
    val A = Point(0.0, 0.0)
    val B = Point(0.0, 2.0)
    val C = Point(1.0, 0.0)
    val E = Point(2.0, 2.0)
    assert(!E.is_incircle(A, B, C), "E n'est pas dans (ABC)")
    assert(!E.is_incircle(C, A, B), "E n'est pas dans (ABC)")
    assert(!E.is_incircle(B, A, C), "E n'est pas dans (ABC)")
    assert(!A.is_incircle(B, C, E), "A n'est pas dans (BCE)")
    assert(B.is_incircle(A, C, E), "B est bien dans (ACE)")

    // Sur le cerle
    assert(Point(1.0, 2.0).is_incircle(A, B, C), "(1,2) est bien dans (ABC)")
    // Pt colineaire
    try {
      B.is_incircle(A, C, Point(2, 0))
    } catch {
      case e: Throwable =>
        assert(e.getMessage().startsWith("assumption failed"))
    }

    // Pt similaire
    try B.is_incircle(A, A, C)
    catch {
      case e: Throwable =>
        assert(e.getMessage().startsWith("assumption failed"))
    }

    assert(A.is_incircle(A, E, C), "A fait partie des points")

    println("test test_incircle() ok")
  }
}
