/** Custom structure to cut an ordered sequence in groups of 2 or 3 consecutive
  * elements.
  */
private sealed trait Tree
private case class Node(left: Tree, right: Tree) extends Tree

/** group of 2 or 3 ordered element */
private case class Leaf(
    fst: FinitePoint,
    snd: FinitePoint,
    trd: Option[FinitePoint]
) extends Tree

private object Tree {

  /** Throw IllegalArgumentException si la liste contient des points dupliqués
    */
  def apply(elements: Iterable[FinitePoint]): Tree = {
    val uniqueElements = elements.toSeq.distinct
    if (uniqueElements.size != elements.size) {
      throw new IllegalArgumentException(
        "Il y a deux points pareil dans la liste à trianguler"
      )
    } else {
      val sortedElements = uniqueElements.sorted
      recBuild(sortedElements)
    }
  }

  private def recBuild(elements: Seq[FinitePoint]): Tree = {
    if (elements.length <= 3) {
      Leaf(
        elements(0),
        elements(1),
        if (elements.length == 3) Some(elements(2)) else None
      )
    } else {
      val (leftElem, rightElem) = elements.splitAt(elements.length / 2)
      Node(recBuild(leftElem), recBuild(rightElem))
    }
  }
}
