object Delaunay {
  def TriangulateDelaunay(points: List[FinitePoint]): QuadEdge =
    TriangulateDelaunay(points, true)

  def TriangulateDelaunay(
      points: List[FinitePoint],
      vornoise: Boolean
  ): QuadEdge = {
    val root = Tree.apply(points)
    val (resEdge, _) = divideAndConquerDelaunay(root)
    if (vornoise) setVoronoiDual(resEdge)
    resEdge
  }

  def setVoronoiDual(resEdge: QuadEdge): Unit = {
    resEdge.iterator.foreach(qe => {
      val a = qe
      val b = qe.lnext
      val c = qe.lnext.lnext
      assume(a.dstNotInf == b.orgNotInf)
      assume(b.dstNotInf == c.orgNotInf)
      val cell =
        if (
          c.dstNotInf == a.orgNotInf
          && !FinitePoint.areCollinear(a.orgNotInf, b.orgNotInf, c.orgNotInf)
          && FinitePoint.ccw(a.orgNotInf, b.orgNotInf, c.orgNotInf)
        ) {
          val cc =
            FinitePoint.circumcenter(a.orgNotInf, b.orgNotInf, c.orgNotInf)
          MutableCell(cc)
        } else {
          MutableCell(InfinitePoint)
        }
      qe.tor.setOrigCell(cell)

      //

      val d = qe
      val e = qe.rprev
      val f = qe.rprev.rprev
      assume(d.dstNotInf == e.orgNotInf)
      assume(e.dstNotInf == f.orgNotInf)
      val celltwo =
        if (
          f.dstNotInf == d.orgNotInf
          && !FinitePoint.areCollinear(d.orgNotInf, e.orgNotInf, f.orgNotInf)
          && !FinitePoint.ccw(d.orgNotInf, e.orgNotInf, f.orgNotInf)
        ) {
          val circ =
            FinitePoint.circumcenter(d.orgNotInf, e.orgNotInf, f.orgNotInf)
          MutableCell(circ)
        } else {
          MutableCell(InfinitePoint)
        }
      qe.rot.setOrigCell(celltwo)
    })

    // val edgeSets: Set[Set[QuadEdge]] = resEdge.iterator
    //   .flatMap(e => Seq(e.right_ring().toSet, e.left_ring().toSet))
    //   .toSet

    // if (isTriangle(edgeSets)) { // only 3 points
    //   processSimpleVoronoi(edgeSets)
    // } else {
    //   processComplexVoronoi(edgeSets)
    // }

    def isTriangle(edgeSets: Set[Set[QuadEdge]]): Boolean = {
      edgeSets.size == 2 && edgeSets.forall(_.size == 3)
    }

    def processSimpleVoronoi(edgeSets: Set[Set[QuadEdge]]): Unit = {
      edgeSets.toSeq match {
        case Seq(set1, set2) =>
          val points =
            set1.flatMap(e => Seq(e.tor.orgNotInf, e.tor.dstNotInf)).toSet.toSeq
          points match {
            case Seq(p1, p2, p3) =>
              val circumcenter = MutableCell(
                FinitePoint.circumcenter(p1, p2, p3)
              )
              set1.foreach(_.setOrigCell(circumcenter))
              set1.foreach(_.setDestCell(new MutableCell(InfinitePoint)))
            case _ => println("Unexpected point configuration.")
          }
        case _ => println("Unexpected edge set configuration.")
      }
    }

    def processComplexVoronoi(edgeSets: Set[Set[QuadEdge]]): Unit = {
      edgeSets.foreach { edgeSet =>
        val points =
          edgeSet
            .flatMap(e => Seq(e.rot.orgNotInf, e.rot.dstNotInf))
            .toSet
            .toSeq
        val cell = if (points.size == 3) {
          val circumcenter =
            FinitePoint.circumcenter(points(0), points(1), points(2))
          MutableCell(circumcenter)
        } else {
          MutableCell(InfinitePoint)
        }
        edgeSet.foreach(p => p.setOrigCell(cell))
      }
    }
  }

  /** return the leftmost and right most edge of the hull in clock wise
    * ```txt
    *      o <--- o <--- o
    * this |              ^   and this one
    *      v               \
    *      o --> o --> o -> o
    * ```
    */
  private def divideAndConquerDelaunay(tree: Tree): (QuadEdge, QuadEdge) =
    tree match {
      // (Base case) 2 or 3 points
      case l: Leaf => handleLeaf(l)
      // reccursion
      case Node(left, right) => handleNode(left, right)
    }

  private def handleLeaf(l: Leaf) = l match {
    case Leaf(p1, p2, None) => {
      val a = QuadEdge.make_edge(p1, p2)
      (a, a.sym)
    }

    case Leaf(p1, p2, Some(p3)) => {
      val a = QuadEdge.make_edge(p1, p2)
      val b = QuadEdge.make_edge(p2, p3)
      QuadEdge.splice(a.sym, b)
      // Closing the triangle
      if (FinitePoint.ccw(p1, p2, p3)) {
        val c = b ---> a
        (a, b.sym)
      } else if (FinitePoint.ccw(p1, p3, p2)) {
        val c = b ---> a
        (c.sym, c)
      } else (a, b.sym) // Colinear
    }
  }

  private def handleNode(left: Tree, right: Tree) = {
    var (ldo, ldi) = divideAndConquerDelaunay(left)
    var (rdi, rdo) = divideAndConquerDelaunay(right)

    // Building the base
    val (vldi, vrdi) = Delaunay.findCommonTangeant(ldi, rdi)
    ldi = vldi
    rdi = vrdi
    var basel = rdi.sym ---> ldi
    // var basel = QuadEdge.connect(rdi.sym, ldi)

    if (ldi.org == ldo.org) ldo = basel.sym
    if (rdi.org == rdo.org) rdo = basel

    fusion(basel)

    (ldo, rdo)
  }

  /** Fuse two hull, assumed already linked by the base */
  private def fusion(base: QuadEdge): Unit = {
    @annotation.tailrec
    def recursiveFusion(basel: QuadEdge): Unit = {
      // above the base, like having an angle < 180°
      def valid(e: QuadEdge): Boolean = basel.rightof(e.dstNotInf)

      val (newBasel, updated) = {

        // lcand ^       ^ rcand
        //       |       |
        //       o <---- o
        val possibleRcand = basel.oprev
        val possibleLcand = basel.sym.onext

        // candidates failing the interview are deleted
        val realRcand =
          findRealCandidate(possibleRcand, basel, valid, _.oprev)
        val realLcand =
          findRealCandidate(possibleLcand, basel.sym, valid, _.onext)

        if (!valid(realLcand) && !valid(realRcand)) {
          (basel, false)
        } else {
          val newBaselEdge =
            linkBestCandidate(basel, realLcand, realRcand, valid)
          (newBaselEdge, true)
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

    val (baseOrg, baseDst) = (baseEdge.orgNotInf, baseEdge.dstNotInf)

    // he's on steroid
    // If true than current is worst than the next one
    def superInValid(current: QuadEdge, next: QuadEdge) = {
      valid(current) && // bit useless, might be avoid
      next != baseEdge && valid(next) && // "is the next one is cool too"
      // "Is he better than you ?"
      next.dstNotInf.is_incircle(baseDst, baseOrg, current.dstNotInf)
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
  ): QuadEdge = {
    val (lorg, ldst) = (lcand.orgNotInf, lcand.dstNotInf)
    val (rorg, rdst) = (rcand.orgNotInf, rcand.dstNotInf)

    if (
      !valid(lcand) ||
      (valid(rcand) && rdst.is_incircle(ldst, lorg, rorg))
    ) {
      QuadEdge.connect(rcand, basel.sym)
    } else if (
      !valid(rcand) ||
      (valid(lcand) && ldst.is_incircle(rdst, rorg, lorg))
    ) {
      QuadEdge.connect(basel.sym, lcand.sym)
    } else { // unreachable
      throw new RuntimeException(
        "error in the choice of candidate during triangulation"
      )
    }
  }

  @annotation.tailrec
  def findCommonTangeant(ldi: QuadEdge, rdi: QuadEdge): (QuadEdge, QuadEdge) = {
    if (ldi.leftof(rdi.orgNotInf)) {
      findCommonTangeant(ldi.lnext, rdi)
    } else if (rdi.rightof(ldi.orgNotInf)) {
      findCommonTangeant(ldi, rdi.rprev)
    } else {
      (ldi, rdi)
    }
  }

}
