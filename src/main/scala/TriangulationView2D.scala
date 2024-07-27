import scalafx.scene.layout.BorderPane
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.{ToolBar, CheckBox, Button, TextField}
import scalafx.scene.paint.Color
import scalafx.scene.control.Label
import scalafx.scene.canvas.GraphicsContext

class Triangulation2DView extends BorderPane {
  private val canvas = new Canvas(600, 450)
  private var quadedge: QuadEdge = _

  // Toolbar components
  private val numPointsField = new TextField {
    text = "15"
  }

  private val generateButton = new Button {
    text = "Generate"
    onAction = _ => generateTriangulation()
    maxWidth_=(50)
  }

  private val drawTriangulationCheckBox = new CheckBox {
    text = "Draw Triangulation"
    selected = true
    onAction = _ => draw()
  }

  private val drawVoronoiCheckBox = new CheckBox {
    text = "Draw Voronoï"
    selected = true
    onAction = _ => draw()
  }

  private val clearButton = new Button {
    text = "Clear"
    onAction = _ => clearCanvas()
  }

  private val toolBar = new ToolBar {
    items = Seq(
      new Label("Number of Points:"),
      numPointsField,
      generateButton,
      drawTriangulationCheckBox,
      drawVoronoiCheckBox,
      clearButton
    )
  }

  top = toolBar
  center = canvas

  canvas.width <== this.width
  canvas.height <== this.height - toolBar.height

  this.width.onChange((_, _, _) => updateCanvasSize())
  this.height.onChange((_, _, _) => updateCanvasSize())
  toolBar.height.onChange((_, _, _) => updateCanvasSize())

  private def draw(): Unit = {
    clearCanvas()
    if (quadedge == null) {
      center = new Label("Nothing to show")
      return
    }

    val delaunayLines = quadedge.iterator.map(e => (e.orgNotInf(), e.dstNotInf())).toSeq
    if (delaunayLines.isEmpty) return

    val (xmin, xmax, ymin, ymax) = getBoundingBox(delaunayLines)
    val (scale, offsetX, offsetY) = getScaleAndOffset(xmin, xmax, ymin, ymax)

    def scaleCo(p: FinitePoint): FinitePoint =
      FinitePoint(
        (p.x - xmin) * scale + offsetX,
        (p.y - ymin) * scale + offsetY
      )

    if (drawTriangulationCheckBox.selected.value)
      drawTriangulation(delaunayLines, scaleCo)
    if (drawVoronoiCheckBox.selected.value) drawVoronoi(scaleCo)
    drawPoints(delaunayLines, scaleCo)
  }

  private def getBoundingBox(
      lines: Seq[(Point, Point)]
  ): (Double, Double, Double, Double) = {
    val (xmin, xmax) = lines
      .flatMap { case (p1: FinitePoint, p2: FinitePoint) => Seq(p1.x, p2.x) }
      .foldLeft((Double.MaxValue, Double.MinValue)) {
        case ((min, max), value) => (Math.min(min, value), Math.max(max, value))
      }
    val (ymin, ymax) = lines
      .flatMap { case (p1: FinitePoint, p2: FinitePoint) => Seq(p1.y, p2.y) }
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
    val canvasWidth = canvas.width.value
    val canvasHeight = canvas.height.value
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
      scaleCo: FinitePoint => FinitePoint
  ): Unit = {
    val gc = canvas.graphicsContext2D
    gc.stroke = Color.Blue

    lines.foreach {
      case (p1: FinitePoint, p2: FinitePoint) => {
        val FinitePoint(ax, ay) = scaleCo(p1)
        val FinitePoint(bx, by) = scaleCo(p2)
        gc.strokeLine(ax, ay, bx, by)
      }
      case _ =>
        throw new RuntimeException("try to draw an inf point in triangulation")
    }
  }

  private def drawVoronoi(scaleCo: FinitePoint => FinitePoint): Unit = {
    val gc = canvas.graphicsContext2D
    gc.stroke = Color.Green

    quadedge.rot.iterator.foreach(e =>
      (e.org(), e.dst()) match {
        case (InfinitePoint, InfinitePoint) => drawMediatrice(e, scaleCo)
        case (a: FinitePoint, b: FinitePoint) => drawVoronoiEdge(gc, a, b, scaleCo)
        case (InfinitePoint, _: FinitePoint) | (_: FinitePoint, InfinitePoint) => 
          drawVoronoiInfiniteLine(e, scaleCo)
        case _ =>
          throw new RuntimeException("Unreachable case encountered. trigview")
      }
    )
  }

  private def drawVoronoiEdge(
      gc: GraphicsContext,
      a: FinitePoint,
      b: FinitePoint,
      scaleCo: FinitePoint => FinitePoint
  ): Unit = {
    val FinitePoint(ax, ay) = scaleCo(a)
    val FinitePoint(bx, by) = scaleCo(b)
    gc.strokeLine(ax, ay, bx, by)
  }

  private def drawMediatrice(
      e: QuadEdge,
      scaleCo: FinitePoint => FinitePoint
  ): Unit = {
    val segment = Segment(e.tor.orgNotInf(), e.tor.dstNotInf())
    val (a, b, c) = segment.perpendicularBisector

    val (f, g) = (FinitePoint(0.0, -c / b), FinitePoint(-c / a, 0.0))
    val (FinitePoint(ax, ay), FinitePoint(bx, by)) = (scaleCo(f), scaleCo(g))

    val gc = canvas.graphicsContext2D
    gc.stroke = Color.Red
    gc.strokeLine(ax, ay, bx, by)
  }

  private def drawVoronoiInfiniteLine(
      e: QuadEdge,
      scaleCo: FinitePoint => FinitePoint
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

    val gc = canvas.graphicsContext2D
    if (walle.rightof(a)) {
      gc.strokeLine(ax, ay, ax + vx, ay + vy)
    } else {
      gc.strokeLine(ax, ay, ax - vx, ay - vy)
    }

  }

  private def drawPoints(
      lines: Seq[(Point, Point)],
      scaleCo: FinitePoint => FinitePoint
  ): Unit = {
    val gc = canvas.graphicsContext2D
    gc.fill = Color.Red
    lines.flatten { case (a, b) => Seq(a, b) }.foreach {
      case p: FinitePoint => {
        val FinitePoint(x, y) = scaleCo(p)
        val r = 2.0
        gc.fillOval(x - r / 2, y - r / 2, r, r)
      }
      case InfinitePoint => throw new RuntimeException("inf point isnt a point")
    }
  }

  private def generateTriangulation(): Unit = {
    val numPoints = numPointsField.text.value.toInt
    val points = FinitePoint.generatePoints(numPoints)
    quadedge = Delaunay.TriangulateDelaunay(points.toList)
    draw()
  }

  private def clearCanvas(): Unit = {
    val gc = canvas.graphicsContext2D
    gc.clearRect(0, 0, canvas.width.value, canvas.height.value)
  }

  private def updateCanvasSize(): Unit = {
    clearCanvas()
    if (quadedge != null) {
      draw()
    }
  }
}

object Triangulation2DView {
  def apply(): Triangulation2DView = new Triangulation2DView()
}
