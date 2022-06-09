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
      case HittingTree(comp, trees) => ThreeDimConcat((for (tree <- trees) yield gatherHittingSets(tree))).filter(_ != List())
  }

  def ThreeDimConcat(list: List[List[List[FOLTerm]]]): List[List[FOLTerm]] = list match {
      case List() => List(List())
      case List(List()) => List(List())
      case xs::xss => xs ++ ThreeDimConcat(xss)
      //case _ => List(List())
  }

  def getDiagnosis(HS: List[List[FOLTerm]]): List[List[FOLTerm]] = HS match {
      case List(List(x)) => List(List(x))
      case HS => for (set1 <- HS; if !(HS.exists(x => {x.forall(set1.contains(_)) && (x.length < set1.length)}))) yield set1 //TODO: fix this line
      //In python: d = [[1,2,3],[2,3],[2,4,3],[4,5],[5]]
      //new_d = [i for i in d if not any(all(c in i for c in b) and len(b) < len(i) for b in d)]
      case _ => List(List())
  }

}
