package slang.instructions

import scala.collection.mutable.ListBuffer

case class Block(instructions: ListBuffer[Node] = ListBuffer())
    extends Node
    with Instruction {
  def addInstruction(instruction: Node) = instructions.append(instruction)
  var scope = Scope()
  def setParentScope(scope: Scope): Unit =
    this.scope.setParentScope(scope)

  def setScope(scope: Scope): Unit =
    this.scope = scope

  def execute(scope: Scope): Instruction = ???
}
