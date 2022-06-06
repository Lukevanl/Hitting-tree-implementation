// import reflect.api.Trees

case class MTree[+T](value: T, children: List[MTree[T]]) {
  def this(value: T) = this(value, List())
  override def toString = "M(" + value.toString + "{" + children.map(_.toString).mkString(",\n") + "})"
}

object TreeStruct {
  def apply[T](value: T) = new MTree(value, List())
  def apply[T](value: T, children: List[MTree[T]]) = new MTree(value, children)

  def makeTreeWarmup(b: Int, d: Int) = d match {
    case 0 => MTree(0, List())
    case d => MTree(0, makeTreeRecWarmup(b, d, d))
  }

  def makeTreeRecWarmup(b : Int, d : Int, init_d : Int): List[MTree[Any]] = d match {
    case 1 => for (x <- List.range(0,b)) yield (MTree(init_d - d + 1, List()))
    case d => for (x <- List.range(0,b)) yield (MTree(init_d - d + 1, makeTreeRecWarmup(b, d-1, init_d)))
  }

  def countNodes(tree: MTree[Any]): Int = tree match {
    case MTree(0, List()) => 0
    case MTree(0, trees) => 1 + trees.length + (for (tree <- trees) yield (countNodes(tree))).sum
    case MTree(_, List()) => -1
    case MTree(_, trees) => trees.length + (for (tree <- trees) yield (countNodes(tree))).sum
  }
}
  