import Diagnosis._
import Conflicts.tpf
import TreeStruct.makeTreeWarmup
import TreeStruct.countNodes
import HittingTree.{makeHittingTree, gatherHittingSets, getDiagnosis}
import gapt.expr.stringInterpolationForExpressions

object Main extends App {
  val tree = makeTreeWarmup(3,3)
  println(countNodes(tree))
 // println(tree)
  println("Running diagnostics with an empty list of broken components..")
  val hs = List()
  val Some(result) = tpf(problem2)
  println(result)
  //Conflict sets of the problem
  val CS_problem3 = List(List(fot"a1", fot"a2") , List(fot"a1", fot"o1"))
  //Generate hitting tree from these conflict sets
  val Htree = makeHittingTree(CS_problem3, List())
  println("Hitting tree:")
  println(Htree)
  //Gather hitting sets from the hitting tree by reading of the leaf values
  val HS = gatherHittingSets(Htree)
  println("Hitting sets read off the tree:")
  println(HS)
  println("Removed supersets:")
  val diagnosis = getDiagnosis(HS)
  println(diagnosis)

  
  //println("Running diagnostics with the previous result assumed to be a broken component..")
  //val Some(result2) = tpf(problem2, List(fot"a1"))
  //println(result2)


}