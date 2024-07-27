import scalafx.scene.layout.Pane
import scalafx.scene.shape.Circle
import scalafx.scene.shape.Line

object Main extends App {
  // Définition de quelques points pour tester
  val points = List(
    Point(0, 0),
    Point(1, 1),
    Point(1, 0),
    Point(0, 2)
    // Point(4, 4)
  )

  val p = Point(0, 2)
  val pex = p.copy()
  println(p eq pex)

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
