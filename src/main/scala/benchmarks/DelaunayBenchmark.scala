import scala.concurrent.{Future, ExecutionContext, Await}
import scala.concurrent.duration._
import scala.util.{Random, Success, Failure}
import System.nanoTime

object DelaunayBenchmark {
  implicit val ec: ExecutionContext = ExecutionContext.global

  def benchmark(numPoints: Int): Future[(Int, Double)] = Future {
    val points = generateRandomPoints(numPoints)
    val startTime = nanoTime()
    Delaunay.TriangulateDelaunay(points)
    val endTime = nanoTime()

    val points2 = generateRandomPoints(numPoints)
    val startTime2 = nanoTime()
    Delaunay.TriangulateDelaunay(points2)
    val endTime2 = nanoTime()

    val points3 = generateRandomPoints(numPoints)
    val startTime3 = nanoTime()
    Delaunay.TriangulateDelaunay(points3)
    val endTime3 = nanoTime()
    (numPoints, (endTime - startTime + endTime2 - startTime2 + endTime3 - startTime3) / (2*1e9)) // Retourne le temps en secondes
  }

  def generateRandomPoints(numPoints: Int): Iterable[FinitePoint] = {
    val random = new Random()
    (1 to numPoints).map { _ =>
      val x = random.nextDouble() * 1000
      val y = random.nextDouble() * 1000
      FinitePoint(x, y)
    }
  }

  def main(args: Array[String]): Unit = {
    val sizes = (1 to 20).map(x => 100000)
    
    // Lancer les benchmarks en parallèle
    val futures = sizes.map(size => benchmark(size))
    
    // Attendre la fin de tous les benchmarks
    val resultsFuture = Future.sequence(futures)

    try {
      // Bloquer jusqu'à ce que tous les futures soient complétés
      val results = Await.result(resultsFuture, Duration.Inf)
      results.foreach { case (size, time) =>
        if (time >= 0) {
          println(s"    ($size, $time),")
        } else {
          println(s"    # Failed to benchmark $size points")
        }
      }
    } catch {
      case e: Exception =>
        println(s"An error occurred: ${e.getMessage}")
    }
  }
}
