import gapt.expr.formula.fol.FOLTerm
import Conflicts.tp
import gapt.expr.formula.Formula
import gapt.expr.stringInterpolationForExpressions
import scala.util.control.Breaks._


case class HittingTree[+T](value: List[FOLTerm], children: List[HittingTree[T]]) {
  def this(value: List[FOLTerm]) = this(value, List())
  override def toString = "HTREE(visited comp: " + value.toString + "{" + children.map(_.toString).mkString(",\n") + "})"
}

object HittingTree {
    /**
     * Create conflict sets by calling the theorem prover until failure with found conflict sets
     * assumed to be broken components
     * @param sd System descirption of the Diagnostic Problem
     * @param comp Components of the Diagnostic Problem
     * @param obs Observations of the Diagnostic Problem
     * @return List of conflict sets where each conflict set is represented as a list of FOLterms
     */
    def generateConflictSets(sd: List[Formula],comp: List[FOLTerm],obs: List[Formula]): List[List[FOLTerm]]= {
    var AllConflictSets = List(tp(sd,comp,obs,List()).get.toList)
    val notError = true
    breakable {
    while(notError) {
      try {
        val cs: List[FOLTerm] = tp(sd,comp,obs,AllConflictSets.flatten).get.toList
        println(AllConflictSets)
        AllConflictSets = AllConflictSets ++ List(cs)
      }
    catch{
      case _: Throwable =>
      val notError = false
      break
    }
      }
    }
      return AllConflictSets
    
  }

  /**
   * Create tree from list of conflict sets (all_cs) where you keep visited components in left of tuple 
   * and children on the right
   * @param all_cs All conflict sets previosuly generated
   * @param visited_comp Keeps track of the visited components in the recursive call
   * @return Recursive HittingTree with all the visited components and children for each node
   */
  def makeHittingTree(all_cs : List[List[FOLTerm]], visited_comp: List[FOLTerm]): HittingTree[Any] = all_cs match {
    case List() => HittingTree(visited_comp, List())
    case List(List()) => HittingTree(visited_comp, List())
    //case List(List(), _) => HittingTree(visited_comp, List())
    case all_cs => HittingTree(visited_comp, (for (cs_component <- all_cs.head) yield (if (visited_comp.contains(cs_component)) HittingTree(visited_comp, List()) else makeHittingTree(all_cs.tail, visited_comp ++ List(cs_component)))))
  }

  /**
   * Read off hitting sets from hitting tree generated in makeHittingTree() 
   * @param hitting_tree The actual tree of type HittingTree
   * @return List of hitting sets where each conflict set is represented as a list of FOLterms
   */
  def gatherHittingSets(hitting_tree: HittingTree[Any]): List[List[FOLTerm]] = hitting_tree match {
      case HittingTree(comp, List()) => List(comp)
      //case HittingTree(comp, List(HittingTree(comp_found, children))) => (for (child <- children) yield gatherHittingSets(child)).concatenate
      case HittingTree(comp, trees) => ThreeDimConcat((for (tree <- trees) yield gatherHittingSets(tree))).filter(_ != List())
  }
  /**
   * Helper function to concat 3 dimensional list into 2 dimensional
   * @param list The three dimensional list
   * @return Two dimensional list where the nested lists are simply concatenated
   */
  def ThreeDimConcat(list: List[List[List[FOLTerm]]]): List[List[FOLTerm]] = list match {
      case List() => List(List())
      case List(List()) => List(List())
      case xs::xss => xs ++ ThreeDimConcat(xss)
  }
  /**
   * Extract minimal hitting sets from a list of hitting sets
   * @param HS The full set of hitting sets
   * @return A list of hitting sets with the supersets filtered out
   * 
   */
  def getDiagnosis(HS: List[List[FOLTerm]]): List[List[FOLTerm]] = HS match {
      case List(List(x)) => List(List(x))
      case HS => for (set1 <- HS; if !(HS.exists(set2 => {set2.forall(set1.contains(_)) && (set2.length < set1.length)}))) yield set1
      case _ => List(List())
  }



  /**
   * Main function which calls the functions above in the correct order and prints+saves their result
   * @param problem The diagnostic problem (SD, COMP, OBS)
   * @return The list of minimal hitting sets of the diagnostic problems (diagnosis)
   * 
   */
  def mainHTalgorithm(problem: (List[Formula], List[FOLTerm], List[Formula])): List[List[FOLTerm]] = {
    val (sd,comp,obs) = problem
    val CS = generateConflictSets(sd,comp,obs)
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
