import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.layout.StackPane
import scalafx.scene.control.Button

object UiMain extends JFXApp3 {
  val WIDTH = 600
  val HEIGHT = 450

  override def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      title = "Mon Application ScalaFX"
      width = WIDTH
      height = HEIGHT

      scene = new Scene {
        root = new Triangulation2DView()
      }
    }
  }

}
