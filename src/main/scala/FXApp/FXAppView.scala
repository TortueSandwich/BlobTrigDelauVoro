import scalafx.scene.layout.BorderPane
import scalafx.scene.control.ToolBar
import scalafx.scene.layout.Pane
import scalafx.scene.layout.Background
import scalafx.scene.layout.BackgroundFill
import scalafx.scene.paint.Color
import scalafx.scene.layout.CornerRadii
import scalafx.scene.control.TextField
import scalafx.scene.control.CheckBox
import scalafx.scene.control.Button
import scalafx.scene.control.Label

class View(model: Model) extends BorderPane {
  private val drawPane = new Pane {
    background = new Background(
      Array(new BackgroundFill(Color.LightGray, CornerRadii.Empty, null))
    )
  }

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

  private val fatrepr = new CheckBox {
    text = "fatrepr"
    selected = false
  }

  private val generateButton = new Button {
    text = "Generate"
  }

  private val fpo = new Button {
    text = "FPO"
  }

  private val toolBar: ToolBar = new ToolBar {
    items = Seq(
      new Label("Number of Points:"),
      numPointsField,
      generateButton,
      drawTriangulationCheckBox,
      drawVoronoiCheckBox,
      fatrepr,
      fpo
    )
  }

  private val bottomBar: ToolBar = new ToolBar {
    items = Seq() // coordinatesLabel)
  }

  // the order is important, so the draw doesnt overlap
  center = drawPane
  top = toolBar
  bottom = bottomBar

}
