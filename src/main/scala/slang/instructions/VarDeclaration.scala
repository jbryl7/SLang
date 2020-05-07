package slang.instructions

case class VarDeclaration(identifier: String) extends Node with Instruction {
  override def execute(scope: Scope): Instruction = this
}
