import scalafx.scene.layout.Pane
import scalafx.scene.shape.Circle
import scalafx.scene.shape.Line

object Main extends App {
  // val points = List(
  //     FinitePoint(-0.1, -0.1),
  //     FinitePoint(2, 0),
  //     FinitePoint(0, 2),
  //     FinitePoint(3, 3),
  //     FinitePoint(2.8, 1),
  //     FinitePoint(1, 2.5),
  // )

  val points = Consts.points
  val edge = Delaunay.TriangulateDelaunay(points.toList)
  println("Triangulé")

  val sup = Consts.sup
  val sup2 : FinitePoint = null
  val edges = edge.iterator.toSeq.distinct
  val qsup = edges.iterator
      .filter(e => (e.orgNotInf == sup || e.dstNotInf == sup) && (sup2 == null || e.orgNotInf == sup2 || e.dstNotInf == sup2))
      .map(q => if (q.dstNotInf() == sup) q.sym else q)
      .toSeq

  println(s"Supression de ${qsup(0).orgNotInf()} :")
  qsup(0).deleteEdgeFromTriangulation()
  println("finito")

  // val (result1, result2) = Delaunay.delaunaise(points)

  // println(result1)
  // println("Resultats : ")
  // result1.printQuadEdgeStructure()
  // println("et")
  // result2.printQuadEdgeStructure()

  // ScalaFXHelloWorld.start()
  // val triangles = QuadEdgeUtils.getAllTriangles(result1)
  // triangles.foreach { case (p1, p2, p3) =>
  // println(s"Triangle: $p1 -> $p2 -> $p3")
  // }
}
