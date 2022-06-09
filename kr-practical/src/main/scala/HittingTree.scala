import gapt.expr.formula.fol.FOLTerm
import Conflicts.tp
import gapt.expr.formula.Formula
import gapt.expr.stringInterpolationForExpressions

case class HittingTree[+T](value: List[FOLTerm], children: List[HittingTree[T]]) {
  def this(value: List[FOLTerm]) = this(value, List())
  override def toString = "HTREE(visited comp: " + value.toString + "{" + children.map(_.toString).mkString(",\n") + "})"
}

object HittingTree {
  //Create tree from list of conflict sets (all_cs) where you keep visited components in left of tuple and children on the right
  def makeHittingTree(all_cs : List[List[FOLTerm]], visited_comp: List[FOLTerm]): HittingTree[Any] = all_cs match {
    case List() => HittingTree(visited_comp, List())
    case List(List()) => HittingTree(visited_comp, List())
    //case List(List(), _) => HittingTree(visited_comp, List())
    case all_cs => HittingTree(visited_comp, (for (cs_component <- all_cs.head) yield (if (visited_comp.contains(cs_component)) HittingTree(visited_comp, List()) else makeHittingTree(all_cs.tail, visited_comp ++ List(cs_component)))))
  }
  //Read off hitting sets from hitting tree generated in makeHittingTree() 
  def gatherHittingSets(hitting_tree: HittingTree[Any]): List[List[FOLTerm]] = hitting_tree match {
      case HittingTree(comp, List()) => List(comp)
      //case HittingTree(comp, List(HittingTree(comp_found, children))) => (for (child <- children) yield gatherHittingSets(child)).concatenate
      case HittingTree(comp, trees) => ThreeDimConcat((for (tree <- trees) yield gatherHittingSets(tree))).filter(_ != List())
  }
  //Helper function to concat 3 dimensional list into 2 dimensional
  def ThreeDimConcat(list: List[List[List[FOLTerm]]]): List[List[FOLTerm]] = list match {
      case List() => List(List())
      case List(List()) => List(List())
      case xs::xss => xs ++ ThreeDimConcat(xss)
  }
  //Remove supersets from list of hitting sets
  def getDiagnosis(HS: List[List[FOLTerm]]): List[List[FOLTerm]] = HS match {
      case List(List(x)) => List(List(x))
      case HS => for (set1 <- HS; if !(HS.exists(set2 => {set2.forall(set1.contains(_)) && (set2.length < set1.length)}))) yield set1
      case _ => List(List())
  }

  def generateConflictSets(problem_number: Int): List[List[FOLTerm]] = problem_number match {
    case 1 => List(List(fot"a1", fot"a2") , List(fot"a1", fot"o1"))
    case 2 => List(List(fot"a1", fot"a2") , List(fot"a1", fot"o1"))
    case 3 => List(List(fot"a1", fot"a2") , List(fot"a1", fot"o1"))

  }

  def mainHTalgorithm(problem: (List[Formula], List[FOLTerm], List[Formula]), problem_number: Int): List[List[FOLTerm]] = {
    //val sd, comp, obs = problem()
    val CS = generateConflictSets(problem_number)  
    println("Conflict sets: ")
    println(CS)
    val HTree = makeHittingTree(CS, List())
    println("Hitting tree: ")
    println(HTree)
    val HS = gatherHittingSets(HTree)
    println("Hitting sets: ")
    println(HS)
    val diagnosis = getDiagnosis(HS)
    println("Minimal hitting sets:")
    println(diagnosis)
    diagnosis
    
  }
}
