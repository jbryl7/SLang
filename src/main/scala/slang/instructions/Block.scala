package slang.instructions

import scala.collection.mutable.ListBuffer

case class Block(instructions: ListBuffer[Node] = ListBuffer())
    extends Node
    with Instruction {

  def addInstruction(instruction: Node) = instructions.append(instruction)
  var scope = Scope()
  def setParentScope(scope: Option[Scope]): Unit =
    this.scope.setParentScope(scope)

  def setScope(scope: Scope): Unit =
    this.scope = scope

  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}Block:\n${nest} instructions: ${instructions.map(_.toString(nested + 1)).mkString("")}"
  }
  def execute(scope: Scope): Instruction = ???
}
