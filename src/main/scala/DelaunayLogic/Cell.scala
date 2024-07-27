// This is not very useful, it is to use a reference for quadedges that have the same origin

sealed trait Cell[+T] {
  def elem: T
}

case class MutableCell[T](var elem: T) extends Cell[T] {
  def set(e: T): Unit = elem = e
}

case class ImmutableCell[T](elem: T) extends Cell[T]

object Main extends App {
  val mutableCell = MutableCell(42)
  mutableCell.set(100)

  val immutableCell = ImmutableCell(42)
  // immutableCell.elem = 100 // ceci n'est pas permis
}
