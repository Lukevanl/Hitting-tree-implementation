import Diagnosis._
import Conflicts.tpf
import TreeStruct.makeTreeWarmup
import TreeStruct.countNodes
import HittingTree.{makeHittingTree, gatherHittingSets, getDiagnosis, mainHTalgorithm}
import gapt.expr.stringInterpolationForExpressions

object Main extends App {
  val tree = makeTreeWarmup(3,3)
  println(countNodes(tree))
 // println(tree)
  println("Running diagnostics with an empty list of broken components..")
  val hs = List()
  val Some(result) = tpf(problem3)
  println(result)
  //println("Running diagnostics with the previous result assumed to be a broken component..")
  //val Some(result2) = tpf(problem2, List(fot"a1"))
  //println(result2)
  val diagnosis = mainHTalgorithm(problem3)






}