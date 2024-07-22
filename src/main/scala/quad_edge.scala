import scala.collection.mutable.ArrayBuffer
import java.util.ArrayList
import java.util.LinkedList
import scala.util.Random
import scala.util.control.Breaks._
import scala.collection.mutable
import scala.annotation.tailrec

case class Cell[T](var elem: T) {
  def set(e: T) = elem = e
  def print_hash() = {
    println(getClass().getName() + "@" + Integer.toHexString(hashCode()))
  }
}

/** ```
  * Lnext | Dnext         Dprev | Rprev
  * <---  . <---           ---> . --->
  *       ^                     |
  *       |                     v
  * <---  . <---           ---> . --->
  * Onext | Rnext         Lprev | Oprev
  * ```
  */
class QuadEdge(var orig: Cell[Point], var next: QuadEdge, var _rot: QuadEdge)
    extends Iterable[QuadEdge] {

  override def iterator: Iterator[QuadEdge] = {
    val v = scala.collection.mutable.Set[QuadEdge]()
    val stack = scala.collection.mutable.Stack[QuadEdge](this)

    while (stack.nonEmpty) {
      val e = stack.pop()
      if (!v.contains(e) && !v.contains(e.sym())) {
        v.add(e)
        stack.push(e.onext())
        stack.push(e.lnext())
        stack.push(e.dnext())
        stack.push(e.rnext())
      }
    }

    v.iterator
  }

  def printAll() = {
    println(lnext() + "   <-   " + get_dst() + "   <-   " + dnext())
    println("                                         ^")
    println("                                         |")
    println(onext() + "   <-   " + get_org() + "   <-   " + rnext())
  }

  def printAllDual() = {
    println(rot.lnext + "   <-   " + rot.get_dst + "   <-   " + rot.dnext)
    println("                                         ^")
    println("                                         |")
    println(rot.onext + "   <-   " + rot.get_org + "   <-   " + rot.rnext)
  }

  def get_org_cell(): Cell[Point] = orig
  def get_org(): Point = orig.elem
  // eDest = e Sym  Org
  def get_dst_cell(): Cell[Point] = sym().get_org_cell()
  def get_dst(): Point = sym().get_org()
  // e Left = e Rot-‘Org
  def left(): Point = this.tor().get_org()
  def left_cell(): Cell[Point] = this.tor().get_org_cell()
  // eRight = e Rot  Org
  def right(): Point = this.rot().get_org()
  def right_cell(): Cell[Point] = this.rot().get_org_cell()

  def rot(): QuadEdge = _rot
  def sym(): QuadEdge = rot.rot
  def tor(): QuadEdge = this.rot.rot.rot
  // def rotInv(): QuadEdge = _rot.sym()

  def onext(): QuadEdge = next
  def dnext(): QuadEdge = sym().onext().sym()
  def lnext(): QuadEdge = tor().onext().rot()
  def rnext(): QuadEdge = _rot.onext().tor()

  def oprev(): QuadEdge = _rot.onext().rot()
  def dprev(): QuadEdge = tor().onext().tor()
  def lprev(): QuadEdge = next.sym()
  def rprev(): QuadEdge = sym().onext()

  def setNext(new_next: QuadEdge) = next = new_next
  def setOrig(org: Point) = orig.set(org)
  def setOrigCell(org: Cell[Point]) = orig = org
  def setDest(dest: Point) = sym().setOrig(dest)
  def setDestCell(org: Cell[Point]) = sym().setOrigCell(org)

  override def toString(): String = s"$get_org -> ${get_dst()}"

  def deleteEdge() = {
    QuadEdge.splice(this, this.oprev())
    QuadEdge.splice(this.sym(), this.sym.oprev())
  }

  def org_ring(): List[QuadEdge] = {
    @tailrec
    def helper(curr: QuadEdge, acc: List[QuadEdge]): List[QuadEdge] = {
      if (acc.contains(curr)) acc
      else helper(curr.onext(), curr :: acc)
    }
    helper(this, Nil)
  }

  def dest_ring(): List[QuadEdge] = this.sym().org_ring()
  def left_ring(): List[QuadEdge] = this.tor().org_ring()
  def right_ring(): List[QuadEdge] = this.rot().org_ring()

  def same_org_ring(rhs: QuadEdge) =
    this.org_ring().toSet == rhs.org_ring().toSet
  def same_dst_ring(rhs: QuadEdge) =
    this.sym().org_ring().toSet == rhs.sym().org_ring().toSet
  def same_right_ring(rhs: QuadEdge) = this.rot().same_org_ring(rhs.rot())
  def same_left_ring(rhs: QuadEdge) = this.tor().same_org_ring(rhs.tor())

  def maj_ring() = {
    val r = this.right_ring()
    if (is_closed_loop(r.map(_.tor))) {
      val cr = Point.centroid(r.map(_.tor.orig.elem).toSet.toList)
      val celcr = Cell(cr)
      r.foreach(_.setOrigCell(celcr))
    }

    val l = this.left_ring()
    if (is_closed_loop(l.map(_.rot))) {
      val cl = Point.centroid(l.map(_.rot.orig.elem).toSet.toList)
      val celcl = Cell(cl)
      l.foreach(_.setOrigCell(celcl))
    }
  }

  def is_closed_loop(ring: List[QuadEdge]): Boolean = {
    @scala.annotation.tailrec
    def checkLoop(
        remaining: List[QuadEdge],
        visited: mutable.Set[QuadEdge]
    ): Boolean = {
      remaining match {
        case Nil => true
        case head :: tail =>
          if (visited.contains(head) || visited.contains(head.sym())) {
            false
          } else {
            visited.add(head)
            checkLoop(tail, visited)
          }
      }
    }
    checkLoop(ring, mutable.Set[QuadEdge]())
  }

  def splice(rhs: QuadEdge) = QuadEdge.splice(this, rhs)

  /** Splice */
  def $$(rhs: QuadEdge) = this splice rhs

  def connectTo(rhs: QuadEdge): QuadEdge = QuadEdge.connect(this, rhs)

  /** Connect */
  def --->(rhs: QuadEdge) = this connectTo rhs

};

object QuadEdge {
  def apply(src: Point, dst: Point): QuadEdge = make_edge(src, dst)

  def make_edge(src: Point, dst: Point): QuadEdge = {
    val site_voro =
      // Cell(Point(-10, -10))
      // Cell(Point(Random.nextInt(), -200))
      Cell(Point.Infinity)

    val q0 = new QuadEdge(Cell(src), null, null);
    val e0 = new QuadEdge(site_voro, null, null);
    val q1 = new QuadEdge(Cell(dst), null, null);
    val e1 = new QuadEdge(site_voro, null, null);

    q0.next = q0; q1.next = q1;
    e0.next = e1; e1.next = e0;

    q0._rot = e0; e0._rot = q1;
    q1._rot = e1; e1._rot = q0;

    return q0;
  }

  def splice(a: QuadEdge, b: QuadEdge) = {
    val (alpha, beta) = (a.onext().rot(), b.onext().rot())
    val (t1, t2) = (a.onext(), b.onext())
    a.setNext(t2); b.setNext(t1);

    val (tex1, tex2) = (alpha.onext(), beta.onext())
    alpha.setNext(tex2); beta.setNext(tex1);

    a.maj_ring()
    b.maj_ring()
  }

  def connect(a: QuadEdge, b: QuadEdge): QuadEdge = {
    val e = make_edge(a.get_dst(), b.get_org())
    e.setOrig(a.get_dst())
    e.setDest(b.get_org())

    // println(s"link ${a.get_dst()} -> ${b.get_org()}")
    splice(e, a.lnext())
    splice(e.sym(), b)
    e
  }

  // utilisé dans la version incrementale
  def swap(e: QuadEdge) = {
    val a = e.oprev()
    val b = e.sym().oprev()

    splice(e, a);
    splice(e.sym(), b);
    splice(e, a.lnext());
    splice(e.sym(), b.lnext());

    e.setOrig(a.get_dst());
    e.setDest(b.get_dst());
  }
}
