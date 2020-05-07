package slang.instructions

case class Block() extends Node with Instruction {
  var scope = Scope()
  def setParentScope(scope: Scope): Unit =
    this.scope.setParentScope(scope)

  def setScope(scope: Scope): Unit =
    this.scope = scope

  def execute(scope: Scope): Instruction = ???
}
