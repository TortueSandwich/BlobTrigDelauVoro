sealed trait AVL
case object EmptyAVL extends AVL
case class NodeAVL(point: Point, left: AVL, right: AVL, height: Int) extends AVL

object AVL {
  def insert(root: AVL, point: Point): AVL = 
    {
      println("yess root est un "+root.getClass())
      root match {
    case EmptyAVL => NodeAVL(point, EmptyAVL, EmptyAVL, 1)
    case NodeAVL(p, left, right, h) =>
      {
        println("ici")
        if (point < p) {
        val newLeft = insert(left, point)
        balance(
          NodeAVL(p, newLeft, right,
            1 + math.max(height(newLeft), height(right))
          ), point
        )
      } else {
        val newRight = insert(right, point)
        balance(
          NodeAVL(p, left, newRight,
            1 + math.max(height(left), height(newRight))
          ),
          point
        )
      }
    }
  }
}

  private def height(avl: AVL): Int = avl match {
    case EmptyAVL            => 0
    case NodeAVL(_, _, _, h) => h
  }

  private def balance(node: NodeAVL, point: Point): AVL = {
    val NodeAVL(_, _, _, balanceFactor) = node
    if (balanceFactor > 1 && point < node.left.asInstanceOf[NodeAVL].point) {
      // Left Left Case
      rotateRight(node)
    } else if (
      balanceFactor < -1 && point > node.right.asInstanceOf[NodeAVL].point
    ) {
      // Right Right Case
      rotateLeft(node)
    } else if (
      balanceFactor > 1 && point > node.left.asInstanceOf[NodeAVL].point
    ) {
      // Left Right Case
      node.copy(left = rotateLeft(node.left.asInstanceOf[NodeAVL]))
      rotateRight(node)
    } else if (
      balanceFactor < -1 && point < node.right.asInstanceOf[NodeAVL].point
    ) {
      // Right Left Case
      node.copy(right = rotateRight(node.right.asInstanceOf[NodeAVL]))
      rotateLeft(node)
    } else {
      // No rotation needed
      node
    }
  }

  private def rotateRight(node: NodeAVL): NodeAVL = {
    val NodeAVL(p, left, right, _) = node
    val newRoot = left.asInstanceOf[NodeAVL]
    val newLeft = newRoot.right
    val newRight =
      NodeAVL(p, newLeft, right, 1 + math.max(height(newLeft), height(right)))
    NodeAVL(
      newRoot.point,
      newRoot.left,
      newRight,
      1 + math.max(height(newRoot.left), height(newRight))
    )
  }

  private def rotateLeft(node: NodeAVL): NodeAVL = {
    val NodeAVL(p, left, right, _) = node
    val newRoot = right.asInstanceOf[NodeAVL]
    val newRight = newRoot.left
    val newLeft =
      NodeAVL(p, left, newRight, 1 + math.max(height(left), height(newRight)))
    NodeAVL(
      newRoot.point,
      newLeft,
      newRoot.right,
      1 + math.max(height(newLeft), height(newRoot.right))
    )
  }

  private def balanceFactor(node: NodeAVL): Int = node match {
    case NodeAVL(_, left, right, _) => height(left) - height(right)
  }

  def size(root: AVL): Int = root match {
    case EmptyAVL                   => 0
    case NodeAVL(_, left, right, _) => 1 + size(left) + size(right)
  }

  def inOrder(root: AVL): List[Point] = root match {
    case EmptyAVL => Nil
    case NodeAVL(point, left, right, _) =>
      inOrder(left) ::: List(point) ::: inOrder(right)
  }

}
