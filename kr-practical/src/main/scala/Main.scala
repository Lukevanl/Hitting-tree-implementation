import Diagnosis._
import Conflicts.tpf
import TreeStruct.makeTreeWarmup
import TreeStruct.countNodes
import HittingTree.{makeHittingTree, gatherHittingSets, getDiagnosis, mainHTalgorithm}
import gapt.expr.stringInterpolationForExpressions
/**
 * @author Luke van Leijenhorst
 * @author Willem Blokland
 * @author Youri Joosten
 */
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


  /**
   * Single function call for the makeHittingTree algorithm,
   * takes a diagnostic problem as input and gets a diagnosis as output
   */
    val diagnosis = mainHTalgorithm(problem1)
}