package slang.instructions

case class Return(returnStatement: Instruction) extends Node with Instruction {
  override def execute(scope: Scope): Instruction =
    returnStatement.execute(scope) // ???
  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}return\n${nest} ${returnStatement}"
  }
}
