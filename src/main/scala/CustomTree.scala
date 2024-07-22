sealed trait Tree
case class Node(left: Tree, right: Tree) extends Tree
case class Leaf(elements: List[Point]) extends Tree

object Tree {
  def recBuild(elements: List[Point]): Tree = {
    if (elements.length <= 3) {
      Leaf(elements)
    } else {
      val (leftElements, rightElements) = elements.splitAt(elements.length / 2)
      Node(recBuild(leftElements), recBuild(rightElements))
    }
  }

  def apply(elements: List[Point]) = {
    val sortedElements = elements.sorted
    assume(elements.size == elements.toSet.size, "il y a deux points pareil")
    recBuild(sortedElements)
  }
}
