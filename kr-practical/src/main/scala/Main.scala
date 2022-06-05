import Diagnosis._
import Conflicts.tpf
import TreeStruct.makeTree
import TreeStruct.countNodes
import gapt.expr.stringInterpolationForExpressions

object Main extends App {
  val tree = makeTree(3,3)
  println(countNodes(tree))
  println(tree)
  println("Running diagnostics with an empty list of broken components..")
  val hs = List()
  val Some(result) = tpf(problem3)
  println(result)

  //println("Running diagnostics with the previous result assumed to be a broken component..")
  //val Some(result2) = tpf(problem3, result.toList)
  //println(result2)

}