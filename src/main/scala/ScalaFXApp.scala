import scalafx.Includes._

import scalafx.scene.layout.{
  BorderPane,
  Pane,
  CornerRadii,
  BackgroundFill,
  Background
}
import scalafx.scene.control.{ToolBar, Label, TextField, CheckBox, Button}
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Circle, Line}
import scalafx.beans.binding.{Bindings, ObjectBinding}
import javafx.event.EventHandler
import javafx.scene.input.MouseEvent
import scalafx.beans.property.ObjectProperty
import scalafx.beans.property.SetProperty
import scalafx.beans.property.DoubleProperty
import scala.collection.mutable
import scalafx.collections.ObservableHashSet
import scalafx.collections.ObservableArray.Change
import scalafx.beans.property.Property
import scalafx.beans.property.ReadOnlyBooleanProperty

class ScalafxApp extends BorderPane {
  private val drawPane = new Pane {
    background = new Background(
      Array(new BackgroundFill(Color.LightGray, CornerRadii.Empty, null))
    )
  }

  private val selection = new ObservableHashSet[FinitePoint].empty
  private val coordinatesLabel = new Label(resetLabel())
  def resetLabel(): String =
    if (selection.isEmpty) "Nothing"
    else "Selected Points: " + selection.map(formatpt).mkString(", ")

  private val points = ObjectProperty(FinitePoint.generatePoints(15).toSeq)
  private val lines = ObjectProperty(Seq.empty[(FinitePoint, FinitePoint)])

  private var quadedge: ObjectProperty[QuadEdge] = ObjectProperty(
    Delaunay.TriangulateDelaunay(points.get.toList)
  )

  val minSize = Bindings.min(
    drawPane.widthProperty,
    drawPane.heightProperty
  )

  val offsetVec = Bindings.createObjectBinding[FinitePoint](
    () =>
      FinitePoint(
        (drawPane.widthProperty.toDouble - minSize.doubleValue) / 2,
        (drawPane.heightProperty.toDouble - minSize.doubleValue) / 2
      ),
    minSize
  )

  val scalePoint = (p: FinitePoint) =>
    Bindings.createObjectBinding(
      () => offsetVec.value + p * minSize.doubleValue,
      offsetVec,
      minSize
    )

  val scaleFuncTOT = (a: FinitePoint, b: FinitePoint) => {
    val newA = scalePoint(a)
    val newB = scalePoint(b)
    Bindings.createObjectBinding(
      () => clipLineToPane(newA.get, newB.get),
      newA,
      newB
    )
  }

  val getfactory =
    (scaledPts: ObjectBinding[Option[(FinitePoint, FinitePoint)]]) =>
      (functy: ((FinitePoint, FinitePoint)) => Double) =>
        createDoubleBinding(
          () => scaledPts.value.map(functy).getOrElse(0.0),
          scaledPts
        )

  private val numPointsField = new TextField {
    text = "15"
    maxWidth = 50
  }

  private val drawTriangulationCheckBox = new CheckBox {
    text = "Draw Triangulation"
    selected = true
  }

  private val drawVoronoiCheckBox = new CheckBox {
    text = "Draw Voronoï"
    selected = false
  }

  private val pointsVar = new collection.mutable.ListBuffer[FinitePoint]()
  private val circlesVar =
    new collection.mutable.ListBuffer[scalafx.scene.shape.Shape]()

  private val generateButton = new Button {
    text = "Generate"
    onAction = _ => {
      drawPane.children.clear()
      selection.clear()

      val nbPoints = numPointsField.text.value.toInt
      points.set(FinitePoint.generatePoints(nbPoints).toSeq)
      quadedge.set(Delaunay.TriangulateDelaunay(points.get.toList))
      lines.set(
        quadedge.get.iterator.map(e => (e.orgNotInf(), e.dstNotInf())).toSeq
      )

      drawVoronoi()
      drawDelaunay()
      drawPoints()

    }
  }

  private val toolBar: ToolBar = new ToolBar {
    items = Seq(
      new Label("Number of Points:"),
      numPointsField,
      generateButton,
      drawTriangulationCheckBox,
      drawVoronoiCheckBox
    )
  }

  private val bottomBar: ToolBar = new ToolBar {
    items = Seq(coordinatesLabel)
  }

  // the order is important, so the draw doesnt overlap
  center = drawPane
  top = toolBar
  bottom = bottomBar

  def drawPoints() =
    points.value.foreach { point =>
      {
        val p = scalePoint(point)
        val circle: Circle = new Circle {
          radius = 7
          fill <== when(
            Bindings.createBooleanBinding(
              () => selection.contains(point),
              selection
            )
          ) choose Color.Red otherwise Color.Brown
          centerX <== createDoubleBinding(() => p.value.x, p)
          centerY <== createDoubleBinding(() => p.value.y, p)
          onMouseClicked = e => {
            if (selection.contains(point))
              selection.remove(point)
            else selection.add(point)
            drawAdditionalObjects()
          }

          onMouseEntered =
            (e: MouseEvent) => coordinatesLabel.text = formatpt(point)
          onMouseExited =
            (e: MouseEvent) => coordinatesLabel.text = resetLabel()
        }

        drawPane.children.add(circle)
        circle.toFront()
      }
    }

  def drawDelaunay() =
    lines.get.foreach {
      case (p1: FinitePoint, p2: FinitePoint) => {
        val optbinding = scaleFuncTOT(p1, p2)
        val getf = getfactory(optbinding)
        val line = new Line {
          startX <== getf(_._1.x)
          startY <== getf(_._1.y)
          endX <== getf(_._2.x)
          endY <== getf(_._2.y)
          stroke = Color.Blue
          visible <== drawTriangulationCheckBox.selected
          onMouseEntered = (ev: MouseEvent) =>
            coordinatesLabel.text =
              s"Line from ${formatpt(p1)} to ${formatpt(p2)}"
          onMouseExited =
            (e: MouseEvent) => coordinatesLabel.text = resetLabel()
        }
        drawPane.getChildren.add(line)
      }
      case _ =>
        throw new RuntimeException(
          "try to draw an inf point in triangulation"
        )
    }

  def drawVoronoi() =
    quadedge.get.rot.iterator.foreach(e => {
      val l: Line = (e.org(), e.dst()) match {
        case (a: FinitePoint, b: FinitePoint) => {
          val optbinding = scaleFuncTOT(a, b)
          val getf = getfactory(optbinding)
          new Line {
            startX <== getf(_._1.x)
            startY <== getf(_._1.y)
            endX <== getf(_._2.x)
            endY <== getf(_._2.y)
            stroke = Color.Green
            onMouseEntered = (ev: MouseEvent) =>
              coordinatesLabel.text =
                s"Dual line of ${formatpt(e.rot.orgNotInf)} to ${formatpt(e.tor.orgNotInf)}"
            onMouseExited =
              (e: MouseEvent) => coordinatesLabel.text = resetLabel()
          }

        }

        // Mediatrice
        case (InfinitePoint, InfinitePoint) => {
          val segment = Segment(e.tor.orgNotInf(), e.tor.dstNotInf())
          val (a, b, c) = segment.perpendicularBisector
          val (f, g) = (FinitePoint(0.0, -c / b), FinitePoint(-c / a, 0.0))
          val optbinding = scaleFuncTOT(f, g)
          val getf = getfactory(optbinding)
          new Line {
            startX <== getf(_._1.x)
            startY <== getf(_._1.y)
            endX <== getf(_._2.x)
            endY <== getf(_._2.y)
            stroke = Color.Green
            onMouseEntered = (ev: MouseEvent) =>
              coordinatesLabel.text =
                s"Line medatrice of ${formatpt(e.rot.orgNotInf)} to ${formatpt(e.tor.orgNotInf)}"
            onMouseExited =
              (ev: MouseEvent) => coordinatesLabel.text = resetLabel()
          }
        }

        // Demi droite
        case (InfinitePoint, _: FinitePoint) |
            (_: FinitePoint, InfinitePoint) => {
          val (a: FinitePoint, b, walle) = if (e.dst == InfinitePoint) {
            (e.orgNotInf(), e.dst(), e.tor)
          } else {
            (e.dstNotInf(), e.org(), e.sym.tor)
          }
          assume(b == InfinitePoint)
          val (pA, pB, pC) =
            (walle.orgNotInf(), walle.dstNotInf(), walle.oprev().dstNotInf())
          val symPoint = Segment(pA, pB).middle
          val scaledA = scalePoint(a)
          val scaledSym = scalePoint(symPoint)
          val vecDir = Segment(a, symPoint).normalizedDirection

          val width = drawPane.width.value
          val height = drawPane.height.value

          val coefX =
            if (vecDir.x != 0)
              Math.max(
                Math.abs(width / vecDir.x),
                Math.abs(-width / vecDir.x)
              )
            else Double.PositiveInfinity
          val coefY =
            if (vecDir.y != 0)
              Math.max(
                Math.abs(height / vecDir.y),
                Math.abs(-height / vecDir.y)
              )
            else Double.PositiveInfinity

          val coef = Math.min(coefX, coefY) * 1.1

          val endP =
            if (walle.rightof(a)) a + vecDir * coef else a - vecDir * coef

          val optbinding = scaleFuncTOT(a, endP)
          val getf = getfactory(optbinding)
          new Line {
            startX <== getf(_._1.x)
            startY <== getf(_._1.y)
            endX <== getf(_._2.x)
            endY <== getf(_._2.y)
            stroke = Color.Green
            onMouseEntered = (ev: MouseEvent) =>
              coordinatesLabel.text =
                s"Dual line of ${formatpt(e.rot.orgNotInf)} to ${formatpt(e.tor.orgNotInf)}"
            onMouseExited =
              (ev: MouseEvent) => coordinatesLabel.text = resetLabel()
          }
        }
        case _ =>
          throw new RuntimeException("Unreachable case encountered. trigview")
      }
      l.visible <== drawVoronoiCheckBox.selected
      drawPane.getChildren.add(l)

    })

  private def formatpt(p: FinitePoint) = f"(${p.x}%.2f, ${p.y}%.2f)"

  private def clipLineToPane(
      p1: FinitePoint,
      p2: FinitePoint
  ): Option[(FinitePoint, FinitePoint)] = {
    val paneBounds = (
      FinitePoint(0, 0),
      FinitePoint(drawPane.width.value, drawPane.height.value)
    )

    def isInside(point: FinitePoint): Boolean = {
      point.x >= 0 && point.x <= paneBounds._2.x && point.y >= 0 && point.y <= paneBounds._2.y
    }

    def lineIntersection(
        p1: FinitePoint,
        p2: FinitePoint,
        bound: (FinitePoint, FinitePoint)
    ): Option[FinitePoint] = {
      val (b1, b2) = bound
      val denom = (p2.x - p1.x) * (b2.y - b1.y) - (p2.y - p1.y) * (b2.x - b1.x)
      if (denom == 0) return None
      val ua =
        ((b2.x - b1.x) * (p1.y - b1.y) - (b2.y - b1.y) * (p1.x - b1.x)) / denom
      val ub =
        ((p2.x - p1.x) * (p1.y - b1.y) - (p2.y - p1.y) * (p1.x - b1.x)) / denom

      if (ua >= 0 && ua <= 1 && ub >= 0 && ub <= 1) {
        Some(FinitePoint(p1.x + ua * (p2.x - p1.x), p1.y + ua * (p2.y - p1.y)))
      } else None
    }

    val boundaries = Seq(
      (paneBounds._1, FinitePoint(paneBounds._2.x, paneBounds._1.y)), // Top
      (paneBounds._1, FinitePoint(paneBounds._1.x, paneBounds._2.y)), // Left
      (FinitePoint(paneBounds._2.x, paneBounds._1.y), paneBounds._2), // Right
      (FinitePoint(paneBounds._1.x, paneBounds._2.y), paneBounds._2) // Bottom
    )
    val clippedPoints =
      boundaries.flatMap(bound => lineIntersection(p1, p2, bound)).distinct
    if (clippedPoints.size == 2) {
      Some(clippedPoints(0), clippedPoints(1))
    } else if (isInside(p1) && isInside(p2)) {
      Some(p1, p2)
    } else if (isInside(p1)) {
      Some(p1, clippedPoints.headOption.getOrElse(p2))
    } else if (isInside(p2)) {
      Some(clippedPoints.headOption.getOrElse(p1), p2)
    } else {
      None
    }
  }

  private def linePassingBy(p1: FinitePoint, p2: FinitePoint): Line = {
    val segment = Segment(p1, p2)

    val vecDir = segment.normalizedDirection

    val width = drawPane.width.value
    val height = drawPane.height.value

    val coefX =
      if (vecDir.x != 0)
        Math.max(
          Math.abs(width / vecDir.x),
          Math.abs(-width / vecDir.x)
        )
      else Double.PositiveInfinity

    val coefY =
      if (vecDir.y != 0)
        Math.max(
          Math.abs(height / vecDir.y),
          Math.abs(-height / vecDir.y)
        )
      else Double.PositiveInfinity

    val coef = Math.min(coefX, coefY) * 1.1

    val startP = p1 - vecDir * coef
    val endP = p1 + vecDir * coef

    val opt = scaleFuncTOT(startP, endP)
    val getf = getfactory(opt)
    new Line {
      startX <== getf(_._1.x)
      startY <== getf(_._1.y)
      endX <== getf(_._2.x)
      endY <== getf(_._2.y)
      stroke = Color.Black
      onMouseEntered = (ev: MouseEvent) =>
        coordinatesLabel.text =
          s"Line passing by ${formatpt(p1)} and ${formatpt(p2)}"
      onMouseExited = (ev: MouseEvent) => coordinatesLabel.text = resetLabel()
    }

  }

  def drawAdditionalObjects(): Unit = {
    circlesVar.foreach { c =>
      drawPane.children.remove(c)
    }
    circlesVar.clear()

    val selectedPoints = selection.toSeq
    selectedPoints.size match {
      case 2 =>
        val Seq(p1, p2) = selectedPoints
        val line = linePassingBy(p1, p2)
        drawPane.children.add(line)
        circlesVar += line
        line.toBack()

      case 3 => {
        val Seq(a, b, c) = selectedPoints
        val shap = if (FinitePoint.areCollinear(a, b, c)) {
          linePassingBy(a, b)
        } else {
          val center = FinitePoint.circumcenter(a, b, c)
          val scaledcenter = scalePoint(center)
          val getposcenter = (f: FinitePoint => Double) =>
            createDoubleBinding(() => f(scaledcenter.get), scaledcenter)
          val scaledA = scalePoint(a)
          new Circle {
            radius <== Bindings.createDoubleBinding(
              () => Segment(scaledA.get, scaledcenter.get).length,
              scaledA,
              scaledcenter
            )
            centerX <== getposcenter(_.x)
            centerY <== getposcenter(_.y)
            stroke = Color.Black
            fill = Color.Transparent
            onMouseEntered = (ev: MouseEvent) =>
              coordinatesLabel.text =
                s"Circumcenter of ${formatpt(a)}, ${formatpt(b)} and ${formatpt(c)}"
            onMouseExited = (ev: MouseEvent) =>
              coordinatesLabel.text = resetLabel()
          }
        }
        drawPane.children.add(shap);
        circlesVar += shap;
        shap.toBack()
      }
      case _ => ()

    }

  }

}
