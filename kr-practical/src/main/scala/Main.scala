import Diagnosis._
import Conflicts.tpf
import TreeStruct.makeTreeWarmup
import TreeStruct.countNodes
import HittingTree.{makeHittingTree, gatherHittingSets, getDiagnosis, mainHTalgorithm}
import gapt.expr.stringInterpolationForExpressions

object Main extends App {
  /**
   * Warmup exercise:
*/
    val tree = makeTreeWarmup(3,3)
    println("The tree has " + countNodes(tree)+  " nodes. ")
    println(tree)


  println("Running diagnostics with an empty list of broken components..")
  val hs = List()
  val Some(result) = tpf(problem1)
  println(result)

  // This gives an error (None)
//  println("Running diagnostics with the previous result assumed to be a broken component..")
//  val Some(result2) = tpf(problem2, List(fot"a1"))
//  println(result2)

  /**
   * Single function call for the makeHittingTree algorithm,
   * takes a diagnostic problem as input and gets a diagnosis as output
   */
  val diagnosis = mainHTalgorithm(problem3)
}