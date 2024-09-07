import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.layout.StackPane
import scalafx.scene.control.Button
import scalafx.scene.layout.BorderPane

object UiMain extends JFXApp3 {
  val WIDTH = 1200
  val HEIGHT = 600

  // val controller = new Controller(model, view)

  override def start(): Unit = {
    val model = new Model()
    val view = new View(model)

    stage = new JFXApp3.PrimaryStage {
      title = "Mon Application ScalaFX"
      width = WIDTH
      height = HEIGHT

      scene = new Scene {
        root = new ScalafxApp // view
      }
    }
  }

}
