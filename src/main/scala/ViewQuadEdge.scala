import scalafx.scene.layout.Pane
import scalafx.scene.shape.Circle
import scalafx.scene.text.Text
import scalafx.scene.paint.Color
import scalafx.scene.shape.Line
import scalafx.scene.paint.LinearGradient
import scalafx.scene.paint.CycleMethod
import scalafx.scene.paint.Stops
class ViewQuadEdge(val qe: QuadEdge, val infoBar: Text) extends Pane {
  private val paddingQE: Double = 50
  private val points: Set[Point] = qe.iterator.foldLeft(Set[Point]())((s,e) =>  s + e.get_org() + e.get_dst())
  private val lines: Set[(Point, Point)] = qe.iterator.foldLeft(Set[(Point, Point)]())((s,e) =>  s + ((e.get_org(), e.get_dst())))

  // println("Extracted Points: " + points.mkString(", "))
  // println("Extracted Lines: " + lines.mkString(",\n"))

  val minX = points.map(_.x).min
  val maxX = points.map(_.x).max
  val minY = points.map(_.y).min
  val maxY = points.map(_.y).max

  width.onChange(updatePositions())
  height.onChange(updatePositions())
  updatePositions()

  // Draw points
  def drawPoints(
      scaleX: Double,
      scaleY: Double
  ): Seq[javafx.scene.Node] = {
    points.toSeq.flatMap { case Point(px, py) =>
      val mycenterX = paddingQE + (px - minX) * scaleX
      val mycenterY = height.value - paddingQE - (py - minY) * scaleY
      val circle = new Circle {
        centerX = mycenterX
        centerY = mycenterY
        radius = 5
        fill = Color.Red
      }
      val text = new Text {
        this.text = s"(${px.toInt}, ${py.toInt})"
        x = mycenterX + 10
        y = mycenterY
        fill = Color.Black
      }
      Seq(circle)//, text)
    }
  }

  // Draw lines
  def drawLines(
      scaleX: Double,
      scaleY: Double
  ): Seq[javafx.scene.Node] = {
    lines.toSeq.map { case (Point(x1, y1), Point(x2, y2)) =>
      val mystartX = paddingQE + (x1 - minX) * scaleX
      val mystartY = height.value - paddingQE - (y1 - minY) * scaleY
      val myendX = paddingQE + (x2 - minX) * scaleX
      val myendY = height.value - paddingQE - (y2 - minY) * scaleY
      new Line {
        this.startX = mystartX
        this.startY = mystartY
        this.endX = myendX
        this.endY = myendY
        stroke = new LinearGradient(
          startX.value / width.value,
          startY.value / height.value,
          endX.value / width.value,
          endY.value / height.value,
          true,
          CycleMethod.NO_CYCLE,
          Stops(Color.Turquoise, Color.Crimson)
        )
        strokeWidth = 5
        onMouseEntered =
          _ => infoBar.text = s"Line from (${x1}, ${y1}) -> (${x2}, ${y2})"
        onMouseExited = _ => infoBar.text = ""
      }
    }
  }

  // Update the positions of points and lines in the pane
  def updatePositions(): Unit = {

    val (scaleX, scaleY) = (
      if (maxX != minX) (width.value - 2 * paddingQE) / (maxX - minX) else 1.0,
      if (maxY != minY) (height.value - 2 * paddingQE) / (maxY - minY) else 1.0
    )

    children.clear()

    children.addAll(drawLines(scaleX, scaleY))
    children.addAll(drawPoints(scaleX, scaleY))

  }

}

object ViewQuadEdge {}
