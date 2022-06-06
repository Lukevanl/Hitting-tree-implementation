import gapt.expr.formula.fol.FOLTerm
import gapt.expr.formula.Formula
import gapt.expr.stringInterpolationForExpressions

case class HittingTree[+T](value: T, children: List[HittingTree[T]]) {
  def this(value: T) = this(value, List())
  override def toString = "HTREE(visited comp: " + value.toString + "{" + children.map(_.toString).mkString(",\n") + "})"
}

object HittingTree {
  //Create tree where you keep visited components in left of tuple, children on the right
  def makeHittingTree(all_hs : List[List[FOLTerm]], visited_comp: List[FOLTerm]): HittingTree[Any] = all_hs match {
    case List() => HittingTree(visited_comp, List())
    case List(List()) => HittingTree(visited_comp, List())
    //case List(List(), _) => HittingTree(visited_comp, List())
    case all_hs => HittingTree(visited_comp, (for (hs_component <- all_hs.head) yield (makeHittingTree(all_hs.tail, visited_comp ++ List(hs_component)))))
  }

}
