package slang.instructions

case class Return(returnStatement: Instruction) extends Node with Instruction {
  override def execute(scope: Scope): Instruction =
    returnStatement.execute(scope)
}