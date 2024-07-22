case class Cell[T](var elem: T) {
  def set(e: T) = elem = e
  def print_hash() = {
    println(getClass().getName() + "@" + Integer.toHexString(hashCode()))
  }
}
