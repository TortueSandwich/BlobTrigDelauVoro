package DelaunayLogic

import scala.collection.mutable.ArrayBuffer
import java.util.ArrayList
import java.util.LinkedList
import scala.util.Random
import scala.util.control.Breaks._
import scala.collection.mutable
import scala.annotation.tailrec
import GeometricPrimitives._

/** Represents an edge in a subdivision of a space and its dual. A kind of
  * parallel crossed double linked list
  *
  * ```txt
  * Lnext | Dnext         Dprev | Rprev
  * <--- dst <---         ---> dst --->
  *       ^                     ^
  *       |                     |
  * <--- org <---         ---> org --->
  * Onext | Rnext         Lprev | Oprev
  * ```
  *
  * qe.onext.onext.onext.onext... gives every edge that is connected to the Org,
  * all qe, qe.onext, qe.onext.onext have the same Org, it's spinning around Org
  *
  * TTo position yourself on the dual, just call .rot This structure does not
  * depend on the coordinate of the point, only on the relationships between the
  * points and the surfaces
  *
  * ```txt
  *         dst
  *          ^
  *          |
  * left <---+--- right
  *          |
  *         org
  * ```
  */
class QuadEdge(
    private var orig: Cell[Point],
    private var next: QuadEdge,
    private var _rot: QuadEdge
) extends Iterable[QuadEdge] {
  // Why a cell ? cause i wanted to edit the coordinate one time after setting the reference
  // might need some fixes tho

  /** use orgNotInf to get finite point */
  def org: Point = orig.elem

  /** Returns the finite origin point.
    * @throws RuntimeException
    *   if the origin is infinite.
    */
  def org_uncheckinf: FinitePoint = org match {
    case InfinitePoint =>
      throw new RuntimeException("org_uncheckinf giving inf")
    case p: FinitePoint => p
  }

  /** Returns the finite origin point.
    * @throws RuntimeException
    *   if the origin is infinite.
    */
  def orgNotInf: FinitePoint = org_uncheckinf

  /** modifying the element in the cell affects all Quad-edges having the same
    * origin FinitePoint reference
    */
  def org_cell: Cell[Point] = orig

  /** use dstNotInf to get finite point */
  def dst: Point = sym.org

  /** Returns the finite destination point.
    * @throws RuntimeException
    *   if the origin is infinite.
    */
  def dst_uncheckinf: FinitePoint = sym.org match {
    case InfinitePoint =>
      throw new RuntimeException("dst_uncheckinf giving inf")
    case p: FinitePoint => p
  }

  /** Returns the finite destination point.
    * @throws RuntimeException
    *   if the origin is infinite.
    */
  def dstNotInf: FinitePoint = dst_uncheckinf

  /** modifying the element in the cell affects all Quad-edges having the same
    * destination FinitePoint reference
    */
  def dst_cell: Cell[Point] = sym.org_cell

  def left: Point = this.tor.org
  def left_cell: Cell[Point] = this.tor.org_cell

  def right: Point = this.rot.org
  def right_cell: Cell[Point] = this.rot.org_cell

  def rot: QuadEdge = _rot
  def sym: QuadEdge = rot.rot
  def tor: QuadEdge = this.rot.rot.rot
  // def rotInv(): QuadEdge = _rot.sym

  def onext: QuadEdge = next
  def rnext: QuadEdge = rot.onext.tor
  def dnext: QuadEdge = sym.onext.sym
  def lnext: QuadEdge = tor.onext.rot

  def lprev: QuadEdge = onext.sym
  def oprev: QuadEdge = rot.onext.rot
  def rprev: QuadEdge = sym.onext
  def dprev: QuadEdge = tor.onext.tor

  def setNext(new_next: QuadEdge) = next = new_next
  def setOrig(org: Point) = orig match {
    case orig: MutableCell[Point] => orig.set(org)
    case ImmutableCell(element) =>
      throw new RuntimeException("cannot change immutable cell")
  }

  def setOrigCell(org: Cell[Point]) = orig = org
  def setDest(dest: Point) = sym.setOrig(dest)
  def setDestCell(org: Cell[Point]) = sym.setOrigCell(org)

  override def toString: String = s"${org} -> ${dst}"

  override def iterator: Iterator[QuadEdge] = {
    val v = scala.collection.mutable.Set[QuadEdge]()
    val stack = scala.collection.mutable.Stack[QuadEdge](this)

    while (stack.nonEmpty) {
      val e = stack.pop()
      if (!v.contains(e) && !v.contains(e.sym)) {
        v.add(e)
        stack.push(e.onext)
        stack.push(e.lnext)
        stack.push(e.dnext)
        stack.push(e.rnext)
      }
    }

    v.iterator
  }

  /** returns all quad-edges having the same origin (reference) */
  def org_ring(): List[QuadEdge] = {
    @tailrec
    def helper(curr: QuadEdge, acc: List[QuadEdge]): List[QuadEdge] = {
      if (acc.contains(curr)) acc
      else helper(curr.onext, curr :: acc)
    }
    helper(this, Nil)
  }

  /** returns all quad-edges having the same origin reference (not necessary the
    * same FinitePoint)
    */
  def dest_ring(): List[QuadEdge] = this.sym.org_ring()

  /** returns all Quad-edges having the same left reference (not necessary the
    * same FinitePoint) Left is in the dual
    */
  def left_ring(): List[QuadEdge] = this.tor.org_ring()

  /** returns all Quad-edges having the same right reference (not necessary the
    * same FinitePoint) right is in the dual
    */
  def right_ring(): List[QuadEdge] = this.rot.org_ring()

  /** return all FinitePoint */
  def getPoints(): Seq[FinitePoint] =
    this.iterator.flatMap(e => Seq(e.orgNotInf, e.dstNotInf)).toSet.toSeq

  /** delete quad-edge WITHOUT PRESERVING TRIANGULATION properties */
  def deleteEdge(): Unit = {
    QuadEdge.splice(this, this.oprev)
    QuadEdge.splice(this.sym, this.sym.oprev)
  }

  /** returns all the quad-edges that form the hull All the quad-edges exposed
    * to the outer
    *
    * MUST BE TRIANGULATED, otherwise behavior is undefined. Todo
    */
  def getExternalHull(): Set[QuadEdge] = {
    val edgeSets: Set[Set[QuadEdge]] = this.iterator
      .flatMap(e => Seq(e.left_ring().toSet, e.right_ring().toSet))
      .toSet
    val t = edgeSets.iterator.filterNot(_.size == 3).map(_.map(_.rot)).toSeq
    assert(t.size == 1)
    return t(0)
  }

  /** if point is strictly on the right side of the quad-edge */
  def rightof(X: FinitePoint): Boolean = {
    val orgp = this.org match {
      case p: FinitePoint => p
      case InfinitePoint =>
        throw new RuntimeException("cannot pass infinite point")
    }
    val dstp = this.dst match {
      case p: FinitePoint => p
      case InfinitePoint =>
        throw new RuntimeException("cannot pass infinite point")
    }
    FinitePoint.ccw(X, dstp, orgp)
  }

  /** if point is strictly on the left side of the quad-edge */
  def leftof(X: FinitePoint): Boolean = {
    val orgp = this.org match {
      case p: FinitePoint => p
      case InfinitePoint =>
        throw new RuntimeException("cannot pass infinite point")
    }
    val dstp = this.dst match {
      case p: FinitePoint => p
      case InfinitePoint =>
        throw new RuntimeException("cannot pass infinite point")
    }
    FinitePoint.ccw(X, orgp, dstp)
  }

  def getTriangle(): Set[(FinitePoint, FinitePoint, FinitePoint)] = {
    val edgeSets: Set[Set[QuadEdge]] = this.iterator
      .flatMap(e => Seq(e.right_ring().toSet, e.left_ring().toSet))
      .toSet

    edgeSets
      .filter(_.size == 3)
      .map(s => {
        val trigpts = s
          .flatMap(e => Seq(e.rot.orgNotInf, e.rot.dstNotInf))
          .toSet

        trigpts.toSeq match {
          case Seq(a, b, c) => (a, b, c)
        }
      })
  }

  /** returns a COPY version where add point in middle each segment and the
    * center of inscrit circle
    *
    * Not tested. Use at your own risk.
    */
  def fatonise() = {
    val pts = getPoints()
    val mdl =
      this.iterator.map(e => Segment(e.orgNotInf, e.dstNotInf).middle).toSeq

    val edgeSets: Set[Set[QuadEdge]] = this.iterator
      .flatMap(e => Seq(e.right_ring().toSet, e.left_ring().toSet))
      .toSet

    val incirc = edgeSets
      .filter(_.size == 3)
      .map(s => {
        val trigpts = s
          .flatMap(e =>
            Seq(
              e.rot.orgNotInf,
              e.rot.dstNotInf,
              e.tor.orgNotInf,
              e.tor.dstNotInf
            )
          )
          .toSet

        trigpts.toSeq match {
          case Seq(a, b, c) if !FinitePoint.areCollinear(a, b, c) =>
            FinitePoint.incenter(a, b, c)
        }
      })

    val newFat =
      Delaunay.TriangulateDelaunay(pts.concat(mdl).concat(incirc).toSet.toList)
    newFat
  }

  // in developement
  private def deleteEdgeFromTriangulationSPECIAL(): Unit = {
    val toDelete = this.orgNotInf
    val orgRing = this.org_ring()
    val externalHullEdges = getExternalHull()
    val externalNeighbors = orgRing
      .filter(e =>
        externalHullEdges.contains(e) || externalHullEdges.contains(e.sym)
      )
      .sortBy(_.dstNotInf)(CounterClockwiseComparator(toDelete))
    if (externalNeighbors.size != 2) {
      throw new RuntimeException("Expected exactly two external neighbors.")
    }
    val (startEdge, endEdge) = (externalNeighbors.head, externalNeighbors.last)
    val startPoint = startEdge.dstNotInf
    val endPoint = endEdge.dstNotInf
    val (beforeStart, afterStart) = orgRing.span(_.dstNotInf != startPoint)
    val rearrangedEdges = afterStart ++ beforeStart
    (rearrangedEdges).foreach(println)
    val pots = rearrangedEdges.map(_.dstNotInf)
    def isCCW(
        prev: FinitePoint,
        mid: FinitePoint,
        next: FinitePoint
    ): Boolean = {
      FinitePoint.ccw(prev, mid, next)
    }
    // println(s"pots $pots")

    val ccwSubLists: List[List[(FinitePoint, FinitePoint, FinitePoint)]] =
      pots
        .sliding(3)
        .collect { case List(p, c, n) => (p, c, n) }
        .foldLeft(List[List[(FinitePoint, FinitePoint, FinitePoint)]]()) {
          (acc, cpl) =>
            cpl match {
              case (p, c, n) if !isCCW(p, c, n) =>
                acc match {
                  case Nil =>
                    List(List((p, c, n)))
                  case head :: tail =>
                    if (head.last._2 == p) {
                      (head :+ (p, c, n)) :: tail
                    } else {
                      List((p, c, n)) :: acc
                    }
                }
              case _ => acc
            }
        }
        .reverse

    ccwSubLists.foreach { subList =>
      println("Sous-liste CCW maximale:")
      subList.foreach(println)
    }

    // closing
    val lastPoint = rearrangedEdges.last.dstNotInf
    val firstPoint = rearrangedEdges.head.dstNotInf
    val closingEdge = QuadEdge.make_edge(lastPoint, firstPoint)
    val pointsToCheck = pots.drop(1).dropRight(1)
    if (pointsToCheck.forall(p => closingEdge.rightof(p))) {
      QuadEdge.connect(endEdge.dnext, startEdge.lnext)
    }

    rearrangedEdges.foreach(_.deleteEdge())

  }

  // in developement
  private def calculateCircumcenterDistance(
      preve: QuadEdge,
      e: QuadEdge,
      nexte: QuadEdge
  ): ((QuadEdge, QuadEdge, QuadEdge), Double) = {
    val prev = preve.dstNotInf
    val curr = e.dstNotInf
    val next = nexte.dstNotInf
    val circ = if (FinitePoint.areCollinear(prev, curr, next)) {
      Segment(prev, next).middle
    } else FinitePoint.circumcenter(curr, next, prev)
    ((preve, e, nexte), FinitePoint.euclidian(e.dstNotInf, circ))
  }

  // in developement (case of quad edge of the external hull)
  def deleteEdgeFromTriangulation(): Unit = {
    val ptsFromHull =
      getExternalHull().flatMap(q => q.orgNotInf :: q.dstNotInf :: Nil).toSet
    if (ptsFromHull.contains(this.orgNotInf)) {
      println("special case")
      this.deleteEdgeFromTriangulationSPECIAL()
    } else {
      println("cas Facile")
      val toDelete = this.orgNotInf
      val orgRing = this.org_ring()
      var orgRingEdges =
        orgRing.sortBy(_.dstNotInf)(CounterClockwiseComparator(toDelete))
      val kp = orgRingEdges.size

      while (orgRingEdges.size > 3) {
        val k = orgRingEdges.size
        val idxs = (1 to k).map(i => Seq(i - 1, i % k, (i + 1) % k))
        val m = idxs.map(cpl => cpl.map(orgRingEdges.apply))
        val edgeData = m.map(cpl => {
          val Seq(p, c, n) = cpl
          val res = calculateCircumcenterDistance(p, c, n)
          res
        })
        val v = edgeData
          .filter(cpl => {
            val es = cpl._1
            val (p, c, n) = es
            FinitePoint
              .ccw(p.dstNotInf, c.dstNotInf, n.dstNotInf) && QuadEdge
              .make_edge(n.dstNotInf, p.dstNotInf)
              .rightof(c.orgNotInf)
          })
          .distinct

        val opt = v.minByOption(_._2)
        opt match {
          case Some((minEdge, _)) =>
            QuadEdge.swap(minEdge._2)
            orgRingEdges = orgRingEdges.filterNot(e => e == minEdge._2)
          case None => {
            orgRingEdges.foreach(_.deleteEdge())
            return
          }
        }
      }
      orgRingEdges.foreach(_.deleteEdge())

    }
  }

  // in developement
  private def deleteEdgeFromTriangulationEX(): Unit = {
    var orgRingEdges = this
      .org_ring()
      .sortBy(_.dstNotInf)(CounterClockwiseComparator(this.orgNotInf))
    println(orgRingEdges.map(_.dstNotInf))
    val k = orgRingEdges.size
    assume(
      (0 to k - 2).forall(i =>
        FinitePoint.ccw(
          orgRingEdges(i + 1).dstNotInf,
          this.orgNotInf,
          orgRingEdges(i).dstNotInf
        )
      ),
      "not sorted in CCW"
    )

    // while (orgRingEdges.size > 3) {

  }

  // in developement
  private def deleteEdgeFromTriangulationEX2(): Unit = {
    def calculateCircumcenterDistance(
        preve: QuadEdge,
        e: QuadEdge,
        nexte: QuadEdge
    ): ((QuadEdge, QuadEdge, QuadEdge), Double) = {
      val prev = preve.dstNotInf
      val curr = e.dstNotInf
      val next = nexte.dstNotInf
      val circ = if (FinitePoint.areCollinear(prev, curr, next)) {
        Segment(prev, next).middle
      } else FinitePoint.circumcenter(curr, next, prev)
      ((preve, e, nexte), FinitePoint.euclidian(e.dstNotInf, circ))
    }
    val toDelete = this.orgNotInf
    val orgRing = this.org_ring()

    var (a: Option[QuadEdge], b: Option[QuadEdge]) = (None, None)
    (0 to orgRing.size - 1).foreach(i => {
      // println(s"${orgRing(i).rprev} , ${orgRing((i+1)%orgRing.size).rprev}")
      if (
        orgRing(i).rprev.dstNotInf != orgRing(
          (i + 1) % orgRing.size
        ).rprev.orgNotInf
      ) {
        // println(s"${orgRing(i).rprev} != ${orgRing((i+1)%orgRing.size).rprev}")
        a = Some(orgRing(i).rprev)
        b = Some(orgRing((i + 1) % orgRing.size).rprev)
      }
    })

    var orgRingEdges =
      orgRing.sortBy(_.dstNotInf)(CounterClockwiseComparator(toDelete))

    val kp = orgRingEdges.size
    // assume(
    // (0 to kp-2).forall(i => FinitePoint.ccw(orgRingEdges(i+1).dstNotInf, this.orgNotInf, orgRingEdges(i).dstNotInf))
    // ,"not sorted in CCW")

    while (orgRingEdges.size > 3) {
      val k = orgRingEdges.size
      val idxs = (1 to k).map(i => Seq(i - 1, i % k, (i + 1) % k))
      val m = idxs.map(cpl => cpl.map(orgRingEdges.apply))
      val edgeData = m.map(cpl => {
        val Seq(p, c, n) = cpl
        val res = calculateCircumcenterDistance(p, c, n)
        res
      })
      val v = edgeData
        .filter(cpl => {
          val es = cpl._1
          val (p, c, n) = es
          FinitePoint
            .ccw(p.dstNotInf, c.dstNotInf, n.dstNotInf) && QuadEdge
            .make_edge(n.dstNotInf, p.dstNotInf)
            .rightof(c.orgNotInf)
        })
        .distinct
      val (minEdge, _) = v.minBy(_._2)
      QuadEdge.swap(minEdge._2)
      orgRingEdges = orgRingEdges.filterNot(e => e == minEdge._2)
    }
    orgRingEdges.foreach(_.deleteEdge())

    if (a != None) {
      // println(s"${a.get} ->> ${b.get}")
      println(a.get.sym.orgNotInf, b.get.orgNotInf, " | ", toDelete)
      println(
        s"LeftOf : ${QuadEdge.make_edge(a.get.sym.orgNotInf, b.get.orgNotInf).leftof(toDelete)}"
      )
      println(
        s"RigthOf : ${QuadEdge.make_edge(a.get.sym.orgNotInf, b.get.orgNotInf).rightof(toDelete)}"
      )
      if (
        QuadEdge
          .make_edge(a.get.sym.orgNotInf, b.get.orgNotInf)
          .rightof(toDelete)
      ) {
        a.get.sym ---> b.get
      }
    }

  }

  /** see QuadEdge.splice */
  def splice(rhs: QuadEdge) = QuadEdge.splice(this, rhs)

  /** Splice QuadEdge.splice
    */
  def $$(rhs: QuadEdge) = this splice rhs

  /** see QuadEdge.connect */
  def connectTo(rhs: QuadEdge): QuadEdge = QuadEdge.connect(this, rhs)

  /** see QuadEdge.connect */
  def --->(rhs: QuadEdge) = this connectTo rhs

};

object QuadEdge {
  def apply(): QuadEdge = make_edge()
  def apply(src: Point, dst: Point): QuadEdge = make_edge(src, dst)

  /** Creation primitive
    *
    * A line in an infinite space. The dual is an infinite line cutting the
    * space in half
    */
  private def make_edge(): QuadEdge = {
    val q0 = new QuadEdge(null, null, null);
    val e0 = new QuadEdge(null, null, null);
    val q1 = new QuadEdge(null, null, null);
    val e1 = new QuadEdge(null, null, null);

    q0.next = q0; q1.next = q1;
    e0.next = e1; e1.next = e0;

    q0._rot = e0; e0._rot = q1;
    q1._rot = e1; e1._rot = q0;

    return q0;
  }

  def make_edge(src: Point, dst: Point): QuadEdge = {
    val qe = make_edge()
    val site_voro = MutableCell(InfinitePoint)

    qe.setOrigCell(MutableCell(src))
    qe.setDestCell(MutableCell(dst))

    qe.rot.setOrigCell(site_voro)
    qe.rot.setDestCell(site_voro)
    qe
  }

  /** Operation primitive
    *
    * Let's be easy
    *
    * a.Org = b.Org <=SPLICE!=> a.Org != b.Org and independently a.left = b.left
    * <=SPLICE!=> a.left != b.left
    *
    * achieved by splitting/fusionning onexts
    */
  def splice(a: QuadEdge, b: QuadEdge) = {
    val (alpha, beta) = (a.onext.rot, b.onext.rot)
    val (t1, t2) = (a.onext, b.onext)
    a.setNext(t2); b.setNext(t1);

    val (tex1, tex2) = (alpha.onext, beta.onext)
    alpha.setNext(tex2); beta.setNext(tex1);

    // a.maj_ring() // useless :|, needs to think about it
    // b.maj_ring() // useless :|, needs to think about it
  }

  /** Connect a.dst to b.org by creating a quad-edge */
  def connect(a: QuadEdge, b: QuadEdge): QuadEdge = {
    val e = make_edge(a.dst, b.org)
    e.setOrig(a.dst)
    e.setDest(b.org)

    // println(s"link ${a.get_dst} -> ${b.get_org}")
    splice(e, a.lnext)
    splice(e.sym, b)
    e
  }

  /** Swap quad-edge "arrow (org->dst) with its dual" used incremental version
    * (which is not implemented)
    */
  def swap(e: QuadEdge) = {
    val a = e.oprev
    val b = e.sym.oprev

    splice(e, a);
    splice(e.sym, b);
    splice(e, a.lnext);
    splice(e.sym, b.lnext);

    e.setOrig(a.dst);
    e.setDest(b.dst);
    Delaunay.setVoronoiDual(e)
  }
}
