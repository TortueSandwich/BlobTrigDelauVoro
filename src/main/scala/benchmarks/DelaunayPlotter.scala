// import scalafx.Includes._
// import scalafx.application.JFXApp3
// import scalafx.application.JFXApp3.PrimaryStage
// import scalafx.scene.Scene
// import scalafx.scene.chart.{LineChart, NumberAxis, XYChart}
// import scalafx.collections.ObservableBuffer
// import scala.util.Random
// import System.nanoTime

// object DelaunayPlotter extends JFXApp3 {

//   def plot(results: Seq[(Int, Double)]): Unit = {
//     // Définir les axes
//     val xAxis = new NumberAxis()
//     xAxis.label = "Nombre de Points"
//     val yAxis = new NumberAxis()
//     yAxis.label = "Temps d'Exécution (secondes)"

//     // Créer le graphique
//     val lineChart = LineChart(xAxis, yAxis)
//     lineChart.title = "Temps d'Exécution de la Triangulation de Delaunay"

//     // Définir une série de données
//     val data = ObservableBuffer(results.map { case (x, y) =>
//       XYChart.Data[Number, Number](x, y)
//     })

//     val series = XYChart.Series[Number, Number]("Temps d'Exécution", data)
//     lineChart.getData.add(series)

//     stage = new PrimaryStage {
//       title = "Graphique du Temps d'Exécution"
//       scene = new Scene(800, 600) {
//         root = lineChart
//       }
//     }
//   }
// }
