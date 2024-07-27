// This is not very useful, it is to use a reference for quadedges that have the same origin

case class Cell[T](var elem: T) {
  def set(e: T) = elem = e
}
