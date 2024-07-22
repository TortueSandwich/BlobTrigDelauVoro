import scala.util.control.Breaks._

object Delaunay {

  def delaunaise(points: List[Point]) = {
    val root = Tree.apply(points)
    println("arbre crée")
    divideAndConquer(root)
  }

  def divideAndConquer(S: Tree): (QuadEdge, QuadEdge) = {
    S match {
      case Leaf(l: List[Point]) =>
        l match {
          case List(p1, p2) => {
            val a = QuadEdge.make_edge(p1, p2)
            (a, a.sym())
          }

          case List(p1, p2, p3) => {
            val a = QuadEdge.make_edge(p1, p2)
            val b = QuadEdge.make_edge(p2, p3)
            QuadEdge.splice(a.sym(), b)
            if (Point.ccw(p1, p2, p3)) {
              val c = QuadEdge.connect(b, a)
              (a, b.sym())
            } else if (Point.ccw(p1, p3, p2)) {
              val c = QuadEdge.connect(b, a)
              (c.sym(), c)
            } else (a, b.sym())
          }
          case _ =>
            throw new RuntimeException(s"Unkowned leaf size : ${l.size}")
        }

      case Node(left, right) => {
        var (ldo, ldi) = divideAndConquer(left)
        var (rdi, rdo) = divideAndConquer(right)
        val q : QuadEdge = new QuadEdge(null, null, null)

        val (vldi, vrdi) = Delaunay.adjustEdges(ldi, rdi)
        ldi = vldi
        rdi = vrdi
        var basel = QuadEdge.connect(rdi.sym(), ldi)
        // println("Fusion !")
        // return (ldo, rdo)

        if (ldi.get_org() == ldo.get_org()) {
          ldo = basel.sym()
        }
        if (rdi.get_org() == rdo.get_org()) {
          rdo = basel
        }

        var i = 0
        breakable {
          while (true) {
            // i += 1
            def valid(e: QuadEdge): Boolean =
              Delaunay.rightof(e.get_dst(), basel)
            // println(s"BASE : $basel")
            var rcand = basel.oprev()
            // println(s"\u001b[92mright : $rcand\u001b[0m")
            if (valid(rcand)) {
              while (
                  rcand.oprev() != basel && valid(rcand.oprev())
                &&
                rcand.oprev().get_dst().is_incircle(
                  basel.get_dst(),
                  basel.get_org(),
                  rcand.get_dst()
                )
                
              ) {
                var t = rcand.oprev()
                rcand.deleteEdge()
                rcand = t
              }
            }




            var lcand = basel.sym().onext()
            // println(s"\u001b[91mleft : $lcand\u001b[0m")
            if (valid(lcand)) {
              while (
                  lcand.onext() != basel.sym() && valid(lcand.onext())
                &&  
                lcand.onext().get_dst().is_incircle(
                  basel.get_dst(),
                  basel.get_org(),
                  lcand.get_dst()
                )
              ) {
                var t = lcand.onext()
                lcand.deleteEdge()
                lcand = t
              }
            }

            
            
            if (!valid(lcand) && !valid(rcand)) {
              // println(s"break when cand are $lcand and $rcand");
              // println(basel.get_dst(), basel.get_org())
              // println(s"${lcand.get_dst()} is rigth of base ? : ${Delaunay
              //     .rightof(lcand.get_dst(), basel)}")
              // println(s"${rcand.get_dst()} is rigth of base ? : ${Delaunay
              //     .rightof(rcand.get_dst(), basel)}")
              break()
            }

            

            if (
              !valid(lcand) || (valid(rcand) &&
                rcand.get_dst().is_incircle(
                  lcand.get_dst(),
                  lcand.get_org(),
                  rcand.get_org()
                ))
            ) {
              // println("right win")
              basel = QuadEdge.connect(rcand, basel.sym())
            } else if ( !valid(rcand) || (valid(lcand) &&
                Point.incircle( rcand.get_dst(), rcand.get_org(), lcand.get_org(), lcand.get_dst()
                ))
            ) {
              // println("left win")
              basel = QuadEdge.connect(basel.sym(), lcand.sym())
            } else {
              println("big erreur")
              break()
            }

          }
        }


        
        
        (ldo, rdo)
      }
    }
  }

  // def deleteEdge(e: QuadEdge) = {
  //   QuadEdge.splice(e, e.oprev())
  //   QuadEdge.splice(e.sym(), e.sym.oprev())
  // }

  def rightof(X: Point, e: QuadEdge): Boolean =
    Point.ccw(X, e.get_dst(), e.get_org())
  def leftof(X: Point, e: QuadEdge): Boolean =
    Point.ccw(X, e.get_org(), e.get_dst())

  @annotation.tailrec
  def adjustEdges(ldi: QuadEdge, rdi: QuadEdge): (QuadEdge, QuadEdge) = {
    if (leftof(rdi.get_org(), ldi)) {
      adjustEdges(ldi.lnext(), rdi)
    } else if (rightof(ldi.get_org(), rdi)) {
      adjustEdges(ldi, rdi.rprev())
    } else {
      (ldi, rdi)
    }
  }
  
}


















