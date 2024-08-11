// This is not very useful, it is to use a reference for quadedges that have the same origin

sealed trait Cell[+T] {
  def elem: T
}

case class MutableCell[T](var elem: T) extends Cell[T] {
  def set(e: T): Unit = elem = e
}

case class ImmutableCell[T](elem: T) extends Cell[T]
