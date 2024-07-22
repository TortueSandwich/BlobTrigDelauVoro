
object TestQuadEdge extends App {
  println("allo")
  // test_make()
  println("lance splice")
  test_splice()

  println("\u001b[92m\u001b[1mToute les assertions sont passées\u001b[0m")
  
  def test_splice() = {
    val (p1, p2, p3) = (Point(0,0), Point(1,1), Point(1,0))
    val env = Cell(Point(80, 80))
    val a = QuadEdge.make_edge(p1, p2)
    val b = QuadEdge.make_edge(p2, p3)

    
    // println(a.printAll())
    // println()
    // println(b.printAll())
    println("SPLICE !")
    // println(a.right_ring())
    QuadEdge.splice(a.sym(), b)

    
    println("------ BEFORE ------")
    println(a.org_ring())
    println(b.org_ring())
    println(a.left_ring().size)
    println(b.left_ring().size)

    println("Connect !")
    val c = QuadEdge.connect(b, a)

    println("------ CONNECTED ------")
    println(a.org_ring())
    println(b.org_ring())
    println(c.org_ring())

    c.deleteEdge()

    println("------ AFTER ------")
    println(a.org_ring())
    println(b.org_ring())
    println(a.left_ring().size)
    println(b.left_ring().size)
    
    
    // println("ajout de d")
    // val p4 = Point(-1,1)
    // val d = QuadEdge.make_edge(p1,p4)
    // QuadEdge.splice(d, a)

    // println(a.rot())
    // println(b.rot())
    // println(c.rot())
    // println(d.rot())
    // println(d.left_ring().size)
    // println(d.right_ring().size)

    // val newqe = QuadEdge.connect(d, b)
    // println(newqe.rot)

    // println("------")
    // println(newqe.rot())
    // println(c.rot())
    // println(d.rot())
    // println(newqe)


    // println(a.left_ring())
    // println(a.right_ring())

    

    // println(a.rot().get_org())
    // println(b.rot().get_org())
    // println(c.rot().get_org())

    // println(a.rot().left() + "  attendu : (0,0)")
    // println(a.rot().right() + "  attendu : (1,0)")

    // a.rot().printAll()
    // println()
    // b.printAll()
    // println()
    // c.printAll()
    // println(a.onext().rot().get_org())
    // println(b.onext().rot().get_org())
    // println(c.onext().rot().get_org())
    // a.onext_ring()
    // b.onext_ring()
    // c.onext_ring()
    // println(a.rot().rot().rot())
    // println(b.rot().rot().rot())
    // println(c.rot().rot().rot())


    // println(a.get_org() eq b.get_org())
    // println(b.left() eq b.right())
    // println(a.left() eq a.right())
    println("ok pour test_splice()")
  }
  def test_other() = {
    val (p1, p2, p3) = (Point(0,0), Point(1,0), Point(1,1))
    val a = QuadEdge.make_edge(p1, p2)
    val b = QuadEdge.make_edge(p2, p3)
    // QuadEdge.touch(a,b)
    println("SPLICE !")
    QuadEdge.splice(a.sym(), b)
    println(a.left() eq a.right())
    println(b.left() eq b.right())
    println(a.left() eq b.left())
    println("-----")
    println(a.get_dst_cell() eq b.get_org_cell())

    QuadEdge.connect(b,a)
    println(a.left() eq a.right())
    println(b.left() eq b.right())
    println(a.left() eq b.left())
    println("-----")
    println(b.get_dst_cell() eq a.get_org_cell())

    // val a = QuadEdge.make_edge(Point(0,0), Point(1,0))
    // val b = QuadEdge.make_edge(Point(0,1), Point(1,1))
    // println(a.left_cell())
    // println(b.left_cell())
    // val ex = QuadEdge.connect(a.sym(),b)
    // val e = QuadEdge.connect(a,b.sym())
    // println(a.left_cell())
    // println(b.left_cell())
    // println(e.left_cell())
    // println("------")
    // a.left_cell().print_hash()
    // b.left_cell().print_hash()
    // e.left_cell().print_hash()
    // println(a.left_cell() eq b.left_cell())
//
    // println("-----")
    // println(a)
    // println(b)
    // println(e)

    // assert(a.left() != null, "0")
    // assert(a.left() eq b.left(), "1")
    // assert(a.left() eq e.left(), "2")
    // assert(b.left() eq e.left(), "3")
    println("ok pour test_other()")
  }

  def test_make() = {
    // eq est l'operateur de test de reference

    val notloop = QuadEdge.make_edge(Point(0, 0), Point(1, 1))
    assert(notloop.lnext() eq notloop.rnext(), "1 not loop")
    assert(notloop.lnext() eq notloop.sym(), "2 not loop")
    assert(notloop.rnext() eq notloop.sym(), "3 not loop")

    assert(notloop.onext() eq notloop.oprev(), "4 not loop")
    assert(notloop.onext() eq notloop, "5 not loop")
    assert(notloop.oprev() eq notloop, "6 not loop")

    // println(notloop.left_cell())
    // println(notloop.right_cell())
    assert(notloop.left_cell() eq notloop.right_cell(), "7 not loop")
    assert(!(notloop.get_org_cell() eq notloop.get_dst_cell()), "8 not loop")

    val loop = QuadEdge.make_edge(Point(-2, -2), Point(-1, -1)).rot()
    assert(loop.lnext() eq loop.rnext(), "1 loop")
    assert(loop.lnext() eq loop, "2 loop")
    assert(loop.rnext() eq loop, "3 loop")

    assert(loop.onext() eq loop.oprev(), "4 loop")
    assert(loop.onext() eq loop.sym(), "5 loop")
    assert(loop.oprev() eq loop.sym(), "6 loop")

    // println(loop.left_cell())
    // println(loop.right_cell())
    assert(!(loop.left_cell() eq loop.right_cell()), "7 loop")
    assert(loop.get_org_cell() eq loop.get_dst_cell(), "8 loop")


    notloop.printAll()

    println("ok pour test_make()")
  }

}
