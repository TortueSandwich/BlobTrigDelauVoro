object FPO {

  def fpoIteration(
      quadedge: QuadEdge,
      ignore: List[FinitePoint]
  ) /*: Double*/ = {
    // var vertexSet = pts.filterNot(ignore.contains)
    // var quadedge = Delaunay.TriangulateDelaunay(pts)
    var tmpquad = quadedge
    val pts = quadedge.getPoints().toSeq

    for (vertex <- pts) {
      var f = vertex
      var rmax = FinitePoint.getLocalMinDist(quadedge.getPoints(), vertex)

      tmpquad = delaunayRemove(tmpquad, vertex)
      val triangles = tmpquad.getTriangle

      for (triangle <- triangles) {
        val circumcenter =
          FinitePoint.circumcenter(triangle._1, triangle._2, triangle._3)
        val circumradius = Segment(circumcenter, triangle._1).length

        if (
          !(circumcenter.x < 0 || circumcenter.x > 1) && !(circumcenter.y < 0 || circumcenter.y > 1) &&
          circumradius > rmax
        ) {
          rmax = circumradius
          f = circumcenter
        }
      }

      tmpquad = delaunayInsert(tmpquad, f)
    }
    println(
      FinitePoint.getAverageMinDist(tmpquad.getPoints()) / FinitePoint
        .getMaxDist(tmpquad.getPoints())
    )
    tmpquad
    // getVertexRatio(quadedge)
  }

// return vertices.getAverageMinDist()/vertices.getMaxDist();

  def delaunayRemove(qe: QuadEdge, pt: FinitePoint): QuadEdge = {
    Delaunay.TriangulateDelaunay(qe.getPoints().filterNot(_ == pt).toList)
  }

  def delaunayInsert(qe: QuadEdge, pt: FinitePoint): QuadEdge = {
    val pts = qe.getPoints()
    val newseq = pts.appended(pt)
    Delaunay.TriangulateDelaunay(newseq.toList)
  }

  def insidePolygon(
      vertex: FinitePoint,
      polygon: List[FinitePoint]
  ): Boolean = {
    if (polygon == null) return true

    val intersections = polygon
      .sliding(2)
      .foldLeft(0)((acc, l) => {
        l match {
          case List(a, b) => {
            if (a.y == b.y) acc
            else if (vertex.y < Math.min(a.y, b.y)) acc
            else if (vertex.y >= Math.max(a.y, b.y)) acc
            else {
              val x = (vertex.y - a.y) * (b.x - a.x) / (b.y - a.y) + a.x
              if (x > vertex.x) acc + 1 else acc
            }
          }
          case _ => throw new RuntimeException("wtf")
        }
      })

    return intersections % 2 == 1;
  }

}
