import scalafx.Includes._

import scalafx.beans.property.DoubleProperty
import scalafx.collections.ObservableHashSet
import scalafx.scene.canvas.Canvas
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.control.Button
import scalafx.scene.control.CheckBox
import scalafx.scene.control.Label
import scalafx.scene.control.Slider
import scalafx.scene.control.TextField
import scalafx.scene.control.ToolBar
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.BorderPane
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color
import scalafx.scene.shape.Circle
import scalafx.scene.shape.Line
import scalafx.scene.text.Text

import scala.collection.mutable
import scala.util.Random

class Triangulation2DView extends BorderPane {
  private val pane = new Pane()
  private var quadedge: QuadEdge = _
  private val pointSize: DoubleProperty = DoubleProperty(8.0)



  private val selection: ObservableHashSet[FinitePoint] = new ObservableHashSet()
  private var selectionPoint: List[Circle] = Nil
  private var selectionMore: List[scalafx.scene.Node] = Nil

  // Toolbar components
  private val numPointsField = new TextField {
    text = "15"
    maxWidth = 50
  }

  private val generateButton = new Button {
    text = "Generate"
    onAction = _ => generateTriangulation()
  }

  private val drawTriangulationCheckBox = new CheckBox {
    text = "Draw Triangulation"
    selected = true
    onAction = _ => {
      // pane.getChildren.clear()
      // draw()
    }
  }

  private val drawVoronoiCheckBox = new CheckBox {
    text = "Draw Voronoï"
    selected = false
    onAction = _ => ()//draw()
  }

  private val clearButton = new Button {
    text = "Clear"
    // onAction = _ => clearPane()
  }

  private val deleteRandomPointButton = new Button {
    text = "Delete Random Point"
    onAction = _ => deleteRandomPoint()
  }
  private val swapRandom = new Button {
    text = "Swap"
    onAction = _ => SwapRandomPoint()
  }

  private val pointSizeSlider = new Slider(0.0, 10.0, 8.0) {
    showTickLabels = true
    showTickMarks = true
    majorTickUnit = 2.0
    minorTickCount = 1
    blockIncrement = 0.1
    value <==> pointSize // Binding the slider value to pointSize property
  }

  pointSize.onChange { (_, _, newValue) =>
    // draw()
  }

  private val toolBar = new ToolBar {
    items = Seq(
      new Label("Number of Points:"),
      numPointsField,
      generateButton,
      drawTriangulationCheckBox,
      drawVoronoiCheckBox,
      clearButton,
      deleteRandomPointButton,
      swapRandom,
      new Label("Point Size:"),
      pointSizeSlider
    )
  }

  top = toolBar
  center = pane

  pane.prefWidthProperty().bind(this.widthProperty())
  pane.prefHeightProperty().bind(this.heightProperty().subtract(toolBar.height))

  this.width.onChange((_, _, _) => updatePaneSize())
  this.height.onChange((_, _, _) => updatePaneSize())
  toolBar.height.onChange((_, _, _) => updatePaneSize())
  updatePaneSize()
  generateTriangulation()

  private def draw(): Unit = {
    // clearPane()
    if (quadedge == null) {
      center = new Label("Nothing to show")
      return
    }

    val delaunayLines =
      quadedge.iterator.map(e => (e.orgNotInf(), e.dstNotInf())).toSeq
    if (delaunayLines.isEmpty) return

    val (xmin, xmax, ymin, ymax) = getBoundingBox(delaunayLines)
    val (scale, offsetX, offsetY) = getScaleAndOffset(xmin, xmax, ymin, ymax)

    scaleCo = (p: FinitePoint) =>
      FinitePoint(
        (p.x - xmin) * scale + offsetX,
        (ymax - p.y) * scale + offsetY // inversion de l'axe y
      )

    if (drawTriangulationCheckBox.selected.value)
      drawTriangulation(delaunayLines)
    if (drawVoronoiCheckBox.selected.value) drawVoronoi()
    drawPoints(delaunayLines)
  }

  private def getBoundingBox(
      lines: Seq[(Point, Point)]
  ): (Double, Double, Double, Double) = {
    val (xmin, xmax) = lines
      .flatMap {
        case (p1: FinitePoint, p2: FinitePoint) => Seq(p1.x, p2.x)
        case _ => throw new RuntimeException("expected finite points")
      }
      .foldLeft((Double.MaxValue, Double.MinValue)) {
        case ((min, max), value) => (Math.min(min, value), Math.max(max, value))
      }
    val (ymin, ymax) = lines
      .flatMap {
        case (p1: FinitePoint, p2: FinitePoint) => Seq(p1.y, p2.y)
        case _ => throw new RuntimeException("expected finite points")
      }
      .foldLeft((Double.MaxValue, Double.MinValue)) {
        case ((min, max), value) => (Math.min(min, value), Math.max(max, value))
      }
    (xmin, xmax, ymin, ymax)
  }

  private def getScaleAndOffset(
      xmin: Double,
      xmax: Double,
      ymin: Double,
      ymax: Double
  ): (Double, Double, Double) = {
    val width = xmax - xmin
    val height = ymax - ymin
    val canvasWidth = pane.width.value
    val canvasHeight = pane.height.value
    val margin = 20
    val scaleX = (canvasWidth - 2 * margin) / width
    val scaleY = (canvasHeight - 2 * margin) / height
    val scale = math.min(scaleX, scaleY)
    val offsetX = (canvasWidth - width * scale) / 2
    val offsetY = (canvasHeight - height * scale) / 2
    (scale, offsetX, offsetY)
  }

  private def drawTriangulation(
      lines: Seq[(Point, Point)],
  ): Unit = {
    lines.foreach {
      case (p1: FinitePoint, p2: FinitePoint) =>
        val FinitePoint(ax, ay) = scaleCo(p1)
        val FinitePoint(bx, by) = scaleCo(p2)
        val line = new Line {
          startX = ax
          startY = ay
          endX = bx
          endY = by
          stroke = Color.Blue
        }
        pane.getChildren.add(line)
      case _ =>
        throw new RuntimeException("try to draw an inf point in triangulation")
    }
  }
  private def drawVoronoi(): Unit = {
    quadedge.rot.iterator.foreach(e =>
      (e.org(), e.dst()) match {
        case (InfinitePoint, InfinitePoint) => drawMediatrice(e)
        case (a: FinitePoint, b: FinitePoint) =>
          drawVoronoiEdge(a, b)
        case (InfinitePoint, _: FinitePoint) |
            (_: FinitePoint, InfinitePoint) =>
          drawVoronoiInfiniteLine(e)
        case _ =>
          throw new RuntimeException("Unreachable case encountered. trigview")
      }
    )
  }

  private def drawVoronoiEdge(
      a: FinitePoint,
      b: FinitePoint,
  ): Unit = {
    val FinitePoint(ax, ay) = scaleCo(a)
    val FinitePoint(bx, by) = scaleCo(b)
    val line = new Line {
      startX = ax
      startY = ay
      endX = bx
      endY = by
      stroke = Color.Green
    }
    pane.getChildren.add(line)
  }

  private def drawMediatrice(
      e: QuadEdge,
  ): Unit = {
    val segment = Segment(e.tor.orgNotInf(), e.tor.dstNotInf())
    val (a, b, c) = segment.perpendicularBisector

    val (f, g) = (FinitePoint(0.0, -c / b), FinitePoint(-c / a, 0.0))
    val (FinitePoint(ax, ay), FinitePoint(bx, by)) = (scaleCo(f), scaleCo(g))

    val line = new Line {
      startX = ax
      startY = ay
      endX = bx
      endY = by
      stroke = Color.Red
    }
    pane.getChildren.add(line)
  }

  private def drawVoronoiInfiniteLine(
      e: QuadEdge,
  ): Unit = {

    val (a: FinitePoint, b, walle) = if (e.dst == InfinitePoint) {
      (e.orgNotInf(), e.dst(), e.tor)
    } else {
      (e.dstNotInf(), e.org(), e.sym.tor)
    }

    assume(b == InfinitePoint)

    val (pA, pB, pC) =
      (walle.orgNotInf(), walle.dstNotInf(), walle.oprev().dstNotInf())

    val symPoint = Segment(pA, pB).middle
    val scaledA = scaleCo(a)
    val FinitePoint(ax, ay) = scaledA
    val scaledSym = scaleCo(symPoint)
    val vecDir = Segment(scaledA, scaledSym).directionVector
    val length = 1000
    val (vx, vy) = (vecDir.x * length, vecDir.y * length)

    val line = new Line {
      startX = ax
      startY = ay
      endX = ax + (if (walle.rightof(a)) vx else -vx)
      endY = ay + (if (walle.rightof(a)) vx else -vy)
      stroke = Color.Red
    }
    pane.getChildren.add(line)
  }

  private def updateVisibility(): Unit = {
    selectionPoint.foreach(c => 
      c.visible = selection.contains(c.userData.asInstanceOf[FinitePoint])
    )
    drawSelection()
  }

  var scaleCo: FinitePoint => FinitePoint = null

  private def drawPoints(
      lines: Seq[(Point, Point)],
  ): Unit = {
    lines.flatten { case (a, b) => a :: b :: Nil }.foreach {
      case p: FinitePoint => {
        val FinitePoint(xp, yp) = scaleCo(p)
        val r = pointSize.value

         val circle = new Circle {
          centerX = xp
          centerY = yp
          radius = r
          fill = Color.Red
          onMouseClicked = e => toggleSelection(p)
          userData = p
        }
              
        val selectionCircle = new Circle {
          centerX = xp
          centerY = yp
          radius = r + 2
          fill = Color.Cyan
          visible = false
          userData = p
        }

        selectionPoint = selectionCircle :: selectionPoint
        
        val text = new Text(f"(${p.x}%.2f, ${p.y}%.2f)") {
          x = xp + r + 5
          y = yp - r - 5
        }

        pane.getChildren.add(selectionCircle)
        pane.getChildren.add(circle)
        pane.getChildren.add(text)
      }
      case InfinitePoint => throw new RuntimeException("inf point isnt a point")
    }
  }

  private def drawSelection(): Unit = {
    selection.toList match {
      case List(p1, p2) =>
        val (a, b, c) = Segment(p1, p2).cartesianEquation

        val (xmin, xmax, ymin, ymax) =
          (0.0, pane.width.value, 0.0, pane.height.value)
        val x1 = 0
        val y1 = (-c / b)
        val x2 = (pane.width.value).toInt
        val y2 = (-c - a * xmax) / b
        val line = new Line {
          startX = x1
          startY = y1
          endX = x2
          endY = y2
          stroke = Color.Cyan
        }
        pane.getChildren.add(line)
      case List(p1, p2, p3) =>
        val circ = FinitePoint.circumcenter(p1, p2, p3)
        val rayon = Segment(p1, circ).length
        val FinitePoint(cx, cy) = scaleCo(circ)
        val circle = new Circle {
          centerX = cx
          centerY = cy
          radius = rayon
          fill = Color.Transparent
          stroke = Color.Cyan
        }
        pane.getChildren.add(circle)
      case _ => ()
    }
  }

  private def generateTriangulation(): Unit = {
    val numPoints = numPointsField.text.value.toInt
    // val points = FinitePoint.generatePoints(numPoints).map(p => {
    //   FinitePoint((p.x*100).toInt, (p.y*100).toInt)
    // })
    // val points = List(
    //   FinitePoint(-1, 0),
    //   FinitePoint(-0.58,1.66),
    //   FinitePoint(-0.22,-1.38),
    //   FinitePoint(1.4,-1.98),
    //   FinitePoint(2.68,-0.6),
    //   FinitePoint(2.24,0.3),
    //   FinitePoint(2.7,1.24),
    //   FinitePoint(1.06,2.36),

    //   FinitePoint(1.34,0.32),
    // )

    // val points = List(
    //   FinitePoint(-1.16,1.96),
    //   FinitePoint(-1.86,-0.26),
    //   FinitePoint(-0.7558607109285,-1.4824784204811),
    //   FinitePoint(1.6057819535745,-1.0786742343226),
    //   FinitePoint(2.0585321016917,1.5032252589942),
    //   FinitePoint(0.3943693950989,2.1762322359252), // <--
    //   FinitePoint(0.68,3.08),
    // )

    // val points = List(
    //   FinitePoint(-0.1, -0.1),
    //   FinitePoint(2, 0),
    //   FinitePoint(0, 2),
    //   FinitePoint(3, 3),
    //   FinitePoint(2.8,1),
    //   FinitePoint(1, 2.5),
    // )

    val points = Consts.points
    quadedge = Delaunay.TriangulateDelaunay(points.toList)
    draw()
  }

  // private def clearPane(): Unit = {
  //   val gc = canvas.graphicsContext2D
  //   gc.clearRect(0, 0, canvas.width.value, canvas.height.value)
  // }

  private def updatePaneSize(): Unit = {
    // clearPane()
    // if (quadedge != null) {
    //   draw()
    // }
  }

  private def deleteRandomPoint(): Unit = {
    if (quadedge == null) {
      println("No triangulation to delete a point from.")
      return
    }

    val points = quadedge.iterator.toSeq.distinct
    if (points.isEmpty) {
      println("No points to delete.")
      return
    }

    // val randomPoint = points(Random.nextInt(points.size))
    // println(points)
    val sup =
      // FinitePoint(2.8, 1.0)
      // FinitePoint(1, 2.5)
      // FinitePoint(2,2)
      Consts.sup
    val randomPoint = points.iterator
      .filter(e => e.orgNotInf == sup || e.dstNotInf == sup)
      .map(q => if (q.dstNotInf() == sup) q.sym else q)
      .toSeq
    // println(s"Deleting point: ${randomPoint(0)}")

    val p = (if (!randomPoint.isEmpty) {
               val p = randomPoint(0)
               println(p)
               p
             } else {
               val p = points((Random.nextInt(points.size) + 1) % points.size)
               println(p)
               p
             })
    if (p == quadedge) {
      println("la chance")
      quadedge = points(Random.nextInt(points.size))
    }
    p.deleteEdgeFromTriangulation()
    // draw()
  }

  private def SwapRandomPoint(): Unit = {
    if (quadedge == null) {
      println("No triangulation to delete a point from.")
      return
    }

    val points = quadedge.iterator.toSeq.distinct
    if (points.isEmpty) {
      println("No points to delete.")
      return
    }

    val randomPoint = points(Random.nextInt(points.size))
    println(s"swap point: $randomPoint")

    QuadEdge.swap(randomPoint)
    // draw()
  }

  private def toggleSelection(p: FinitePoint): Unit = {
    if (!selection.add(p)) {
      selection.remove(p)
    }
    updateVisibility()
  }
}

object Triangulation2DView {
  def apply(): Triangulation2DView = new Triangulation2DView()
}
