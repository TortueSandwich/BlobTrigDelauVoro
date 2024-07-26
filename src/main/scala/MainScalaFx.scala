import scalafx.application.JFXApp3
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.effect.DropShadow
import scalafx.scene.layout.HBox
import scalafx.scene.paint.Color._
import scalafx.scene.paint._
import scalafx.scene.text.Text
import scalafx.scene.layout.Pane
import scalafx.scene.shape.Circle
import scalafx.scene.shape.Line
import scalafx.scene.layout.VBox

object PointsAndLinesApp extends JFXApp3 {

  override def start(): Unit = {
    // val points = List( Point(0, 0), Point(1, 0), Point(1, 1), Point(0, 2))
    // val points = List( Point(-5, -5), Point(-5, 5), Point(-3, 0), Point(5, 5), Point(5, -5), Point(3, 0))
    val points = List(
      // Point(-1,0), Point(-1,1), Point(-0,0),
      // Point(1,0), 
      // Point(2,0), // Point(2,1),
      // Point(4,-1), Point(3,-2), Point(5,-2),
      // Point(10,3), Point(9,1), Point(11,2),


      Point(0,1), Point(1,0), Point(1,2), Point(1,3), Point(2,1), 
      Point(3,3), Point(4,2), Point(5,0), Point(5,1), Point(5,3),

      // Point(-3,3), Point(-4,2), Point(-5,0), Point(-5,1), Point(-5,3),
    )
    val n = 1000
    val result = Delaunay.TriangulateDelaunay(
      // points
      // generateHexagonalLattice(n,n,2)
      Point.generatePoints(1_000).toList
    )
    println("FINIIIIII")
    println(result)
    val truc = result.iterator.flatMap(e => Seq(e.right_ring().toSet, e.left_ring().toSet)).toSet
    truc.foreach(s => if (s.size != 3) println(s.size))
    // println(result2.isDelaunay())



    stage = new JFXApp3.PrimaryStage {
      title = "Points and Lines"
      scene = createScene(result)
    }
  }

  // Create the main scene
  def createScene(qe: QuadEdge): Scene = {
    new Scene(500, 300) {
      val infoBar = new Text("Informations dynamiques")
      val infoBox = new HBox {
        children = infoBar
        padding = Insets(10)
        style = "-fx-background-color: lightgray;"
      }

      val pane = new ViewQuadEdge(qe, infoBar)
      pane.prefWidthProperty().bind(this.widthProperty())
      pane.prefHeightProperty().bind(this.heightProperty().subtract(infoBox.heightProperty()))

      val vbox = new VBox {
        children = Seq(pane, infoBox)
      }

      content = vbox
    }
  }


    def generateHexagonalLattice(rows: Int, cols: Int, a: Double): List[Point] = {
    var points = List[Point]()
    val sqrt3over2 = math.sqrt(3)/ 2

    for (i <- 0 until rows) {
      for (j <- 0 until cols) {
        val x = a * (i + j%2 * 0.5)
        val y = a * (j * sqrt3over2 )
        points = Point(x, y) :: points
      }
    }
    points.toSet.toList
  }

}
