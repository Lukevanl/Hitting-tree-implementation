import Diagnosis._
import Conflicts.tpf
import TreeStruct.makeTreeWarmup
import TreeStruct.countNodes
import HittingTree.makeHittingTree
import gapt.expr.stringInterpolationForExpressions

object Main extends App {
  val tree = makeTreeWarmup(3,3)
  println(countNodes(tree))
 // println(tree)
  println("Running diagnostics with an empty list of broken components..")
  val hs = List()
  val Some(result) = tpf(problem2)
  println(result)
  val HS_problem3 = List(List(fot"a1", fot"a2") , List(fot"a1", fot"o1"))
  // for (hs_component <- HS_problem3.head) yield println(hs_component)
  // println(HS_problem3.tail)
  //val HS_problem3 = List(List(), List())
  //println(HS_problem3)
  val Htree = makeHittingTree(HS_problem3, List())
  println(Htree)
  //println("Running diagnostics with the previous result assumed to be a broken component..")
  //val Some(result2) = tpf(problem2, List(fot"a1"))
  //println(result2)


}