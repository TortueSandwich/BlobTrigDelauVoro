import scala.util.control.Breaks._

object Delaunay {
def TriangulateDelaunay(points: List[Point]): QuadEdge = {
  val root = Tree.apply(points)
  val (resEdge, _) = divideAndConquerDelaunay(root)

  val edgeSets: Set[Set[QuadEdge]] = resEdge.iterator.flatMap(e => Seq(e.right_ring().toSet, e.left_ring().toSet)).toSet

  if (edgeSets.size == 2 && edgeSets.forall(_.size == 3)) {
    edgeSets.toSeq match {
      case Seq(set1, set2) =>
        val pointsInSet1 = set1.flatMap(e => Seq(e.tor.get_org(), e.tor.get_dst())).toSet.toSeq
        pointsInSet1 match {
          case Seq(p1, p2, p3) => {

            val circumcenter = Cell(Point.circumcenter(p1, p2, p3))
            set1.foreach(_.setOrigCell(circumcenter))
            set1.foreach(_.setDestCell(Cell(Point.Infinity)))
          }
          case _ => println("!!!\nWTF22222\n!!!")
        }
      case _ => println("!!!\nWTF\n!!!")
    }
    return resEdge
  }

  edgeSets.foreach { edgeSet =>
    if (edgeSet.size == 3) {
      val points = edgeSet.flatMap(e => Seq(e.rot().get_org(), e.rot().get_dst())).toSet.toSeq
      if (points.size == 3) {
        val circumcenter = Point.circumcenter(points(0), points(1), points(2))
        val cell = Cell(circumcenter)
        edgeSet.foreach(_.setOrigCell(cell))
      }
    } else {
      val cell = Cell(Point.Infinity)
      edgeSet.foreach(_.setOrigCell(cell))
    }
  }

  resEdge
}



  /** return the leftmost and right most edge of the hull in clock wise
    * ```txt
    *      o <-- o <-- o <-- o
    * this |                 ^ and this one
    *      v                 |
    *      o --> o --> o --> o
    * ```
    */
  private def divideAndConquerDelaunay(tree: Tree): (QuadEdge, QuadEdge) =
    tree match {
      case Leaf(points)      => handleLeaf(points)
      case Node(left, right) => handleNode(left, right)
    }

  private def handleLeaf(l: List[Point]) = l match {
    case List(p1, p2) => {
      val a = QuadEdge.make_edge(p1, p2)
      (a, a.sym())
    }

    case List(p1, p2, p3) => {
      val a = QuadEdge.make_edge(p1, p2)
      val b = QuadEdge.make_edge(p2, p3)
      QuadEdge.splice(a.sym(), b)
      // Close the triangle
      if (Point.ccw(p1, p2, p3)) {
        val c = b ---> a
        (a, b.sym())
      } else if (Point.ccw(p1, p3, p2)) {
        val c = b ---> a
        (c.sym(), c)
      } else (a, b.sym()) // Colinear
    }
    case _ =>
      throw new RuntimeException(
        s"Unkowned leaf size : ${l.size}"
      ) // unreachable
  }

  private def handleNode(left: Tree, right: Tree) = {
    var (ldo, ldi) = divideAndConquerDelaunay(left)
    var (rdi, rdo) = divideAndConquerDelaunay(right)

    // Building the base
    val (vldi, vrdi) = Delaunay.findCommonTangeant(ldi, rdi)
    ldi = vldi
    rdi = vrdi
    var basel = rdi.sym() ---> ldi
    // var basel = QuadEdge.connect(rdi.sym(), ldi)

    if (ldi.get_org() == ldo.get_org()) ldo = basel.sym()
    if (rdi.get_org() == rdo.get_org()) rdo = basel

    fusion(basel)

    (ldo, rdo)
  }

  /** Fuse two hull, assumed already linked by the base */
  private def fusion(base: QuadEdge): Unit = {
    @annotation.tailrec
    def recursiveFusion(basel: QuadEdge): Unit = {
      // above the base, like having an angle < 180°
      def valid(e: QuadEdge): Boolean = basel.rightof(e.get_dst())

      val (newBasel, updated) = {

        // lcand ^       ^ rcand
        //       |       |
        //       o <---- o
        val possibleRcand = basel.oprev()
        val possibleLcand = basel.sym().onext()

        // candidates failing the interview are deleted
        val realRcand =
          findRealCandidate(possibleRcand, basel, valid, _.oprev())
        val realLcand =
          findRealCandidate(possibleLcand, basel.sym(), valid, _.onext())

        if (!valid(realLcand) && !valid(realRcand)) {
          (basel, false)
        } else {
          val (newBaselEdge, validCondition) =
            linkBestCandidate(basel, realLcand, realRcand, valid)
          (newBaselEdge, validCondition)
        }
      }

      if (updated) recursiveFusion(newBasel)
    }
    recursiveFusion(base)
  }

  /** also edit, by deleting wrong candidate blocking the way */
  private def findRealCandidate(
      fstCandidate: QuadEdge,
      baseEdge: QuadEdge,
      valid: QuadEdge => Boolean,
      nextEdgeFunc: QuadEdge => QuadEdge
  ): QuadEdge = {

    val (baseOrg, baseDst) = (baseEdge.get_org(), baseEdge.get_dst())

    // he's on steroid
    // If true than current is worst than the next one
    def superInValid(current: QuadEdge, next: QuadEdge) = {
      valid(current) && // bit useless, might be avoid
      next != baseEdge && valid(next) && // "is the next one is cool too"
      next
        .get_dst() // "Is he better than you ?"
        .is_incircle(baseDst, baseOrg, current.get_dst())
    }

    @annotation.tailrec
    def recurse(e: QuadEdge): QuadEdge = {
      // Vérifie si l'edge e est valide pour la condition donnée
      val (currCand, nextCand) = (e, nextEdgeFunc(e))
      if (superInValid(currCand, nextCand)) {
        // then you are out
        val nextE = nextCand
        currCand.deleteEdge()
        recurse(nextE)
      } else {
        currCand
      }
    }
    recurse(fstCandidate)
  }

  /** pits the right-wing and left-wing candidates against each other */
  def linkBestCandidate(
      basel: QuadEdge,
      lcand: QuadEdge,
      rcand: QuadEdge,
      valid: QuadEdge => Boolean
  ): (QuadEdge, Boolean) = {
    val (lorg, ldst) = (lcand.get_org(), lcand.get_dst())
    val (rorg, rdst) = (rcand.get_org(), rcand.get_dst())

    if (
      !valid(lcand) ||
      (valid(rcand) && rdst.is_incircle(ldst, lorg, rorg))
    ) {
      (QuadEdge.connect(rcand, basel.sym()), true)
    } else if (
      !valid(rcand) ||
      (valid(lcand) && ldst.is_incircle(rdst, rorg, lorg))
    ) {
      (QuadEdge.connect(basel.sym(), lcand.sym()), true)
    } else { // unreachable
      println("Erreur dans le choix de l'edge")
      (basel, false)
    }
  }

  @annotation.tailrec
  def findCommonTangeant(ldi: QuadEdge, rdi: QuadEdge): (QuadEdge, QuadEdge) = {
    if (ldi.leftof(rdi.get_org())) {
      findCommonTangeant(ldi.lnext(), rdi)
    } else if (rdi.rightof(ldi.get_org() )) {
      findCommonTangeant(ldi, rdi.rprev())
    } else {
      (ldi, rdi)
    }
  }

}
