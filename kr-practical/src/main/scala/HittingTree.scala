import gapt.expr.formula.fol.FOLTerm
import gapt.expr.formula.Formula
import gapt.expr.stringInterpolationForExpressions

case class HittingTree[+T](value: List[FOLTerm], children: List[HittingTree[T]]) {
  def this(value: List[FOLTerm]) = this(value, List())
  override def toString = "HTREE(visited comp: " + value.toString + "{" + children.map(_.toString).mkString(",\n") + "})"
}

object HittingTree {
  //Create tree where you keep visited components in left of tuple, children on the right
  def makeHittingTree(all_hs : List[List[FOLTerm]], visited_comp: List[FOLTerm]): HittingTree[Any] = all_hs match {
    case List() => HittingTree(visited_comp, List())
    case List(List()) => HittingTree(visited_comp, List())
    //case List(List(), _) => HittingTree(visited_comp, List())
    case all_hs => HittingTree(visited_comp, (for (hs_component <- all_hs.head) yield (if (visited_comp.contains(hs_component)) HittingTree(visited_comp, List()) else makeHittingTree(all_hs.tail, visited_comp ++ List(hs_component)))))
  }

  def gatherHittingSets(hitting_tree: HittingTree[Any]): List[List[FOLTerm]] = hitting_tree match {
      case HittingTree(comp, List()) => List(comp)
      //case HittingTree(comp, List(HittingTree(comp_found, children))) => (for (child <- children) yield gatherHittingSets(child)).concatenate
      case HittingTree(comp, trees) => ThreeDimConcat((for (tree <- trees) yield gatherHittingSets(tree)))
  }

  def ThreeDimConcat(list: List[List[List[FOLTerm]]]): List[List[FOLTerm]] = list match {
      case List() => List(List())
      case List(List()) => List(List())
      case List(xs::xxs) => List(xs) ++ ThreeDimConcat(List(xxs))
      //case _ => List(List())
  }

}
