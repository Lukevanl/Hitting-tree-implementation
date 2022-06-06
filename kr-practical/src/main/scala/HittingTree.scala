import gapt.expr.formula.fol.FOLTerm
import gapt.expr.formula.Formula
import gapt.expr.stringInterpolationForExpressions

case class HittingTree[+T](value: T, children: List[MTree[T]]) {
  def this(value: T) = this(value, List())
  override def toString = "HTREE(" + value.toString + "{" + children.map(_.toString).mkString(",") + "})"
}

object HittingTree {
  def makeHittingTree(ALL_HS : List[List[FOLTerm]]): Any = ALL_HS match {
    case List(List()) => HittingTree(0, List())
    case List(List(x)) => HittingTree(x, List())
    case ALL_HS => HittingTree(0, List())
  }

}
