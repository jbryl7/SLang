package slang.instructions

import scala.collection.mutable.ListBuffer

case class Program(name: String) {
  def addStatement(instruction: Node) = instructions.append(instruction)
  val instructions: ListBuffer[Node] = ListBuffer()
  var currentScope = Scope()
  def destroyLocalScope() = ()
  def addLocalScope(): Scope = Scope()
  def declareNewVar(name: String, varType: MyInt) = () // todo type
  def declareNewFunction = ()
  def getVar(varName: String) = ()
  def getCurrentScope(): Scope = currentScope
}
