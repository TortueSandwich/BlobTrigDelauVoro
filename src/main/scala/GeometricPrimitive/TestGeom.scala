object TestGeom extends App {

  test_colineaire()
  test_CCW()
  // test_incircle()
  println("\u001b[92m\u001b[1mToute les assertions sont passées\u001b[0m")

  def test_CCW(): Unit = {
    val O = FinitePoint(0.0, 0.0)
    val A = FinitePoint(0.0, 1.0)
    val B = FinitePoint(1.0, 0.0)
    val C = FinitePoint(0.0, -1.0)
    assert(!FinitePoint.ccw(O, A, B), "OAB")
    assert(!FinitePoint.ccw(A, B, O), "ABO")
    assert(!FinitePoint.ccw(B, O, A), "BOA")
    assert(FinitePoint.ccw(O, B, A), "OBA")
    assert(!FinitePoint.ccw(O, A, C), "OAC")
    assert(!FinitePoint.ccw(O, B, C), "OBC")

    assert(
      !FinitePoint
        .ccw(FinitePoint(1, 0), FinitePoint(-1, 0), FinitePoint(-2, 2)),
      "Test unitaire"
    )

    println("test test_CCW() ok")
  }

  def test_colineaire(): Unit = {
    val A = FinitePoint(0.0, 0.0)
    val B = FinitePoint(0.0, 2.0)
    val C = FinitePoint(0.0, 5.0)
    val D = FinitePoint(1.0, 0.0)
    assert(FinitePoint.areCollinear(A, B, C), "c'est colineaire1")
    assert(FinitePoint.areCollinear(A, C, B), "c'est colineaire2")
    assert(FinitePoint.areCollinear(C, A, B), "c'est colineaire3")
    assert(!FinitePoint.areCollinear(A, B, D), "ce n'dest pas colinire")
    println("test test_colineaire() ok")
  }

  def test_incircle(): Unit = {
    val A = FinitePoint(0.0, 0.0)
    val B = FinitePoint(0.0, 2.0)
    val C = FinitePoint(1.0, 0.0)
    val E = FinitePoint(2.0, 2.0)
    assert(!E.is_incircle(A, B, C), "E n'est pas dans (ABC)")
    assert(!E.is_incircle(C, A, B), "E n'est pas dans (ABC)")
    assert(!E.is_incircle(B, A, C), "E n'est pas dans (ABC)")
    assert(!A.is_incircle(B, C, E), "A n'est pas dans (BCE)")
    assert(B.is_incircle(A, C, E), "B est bien dans (ACE)")

    // Sur le cerle
    assert(
      FinitePoint(1.0, 2.0).is_incircle(A, B, C),
      "(1,2) est bien dans (ABC)"
    )
    // Pt colineaire
    try {
      B.is_incircle(A, C, FinitePoint(2, 0))
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
