package slang.instructions

case class VariableAssignment(variableCall: VariableCall, newValue: Node)
    extends Instruction
    with Node {
  override def execute(scope: Scope): Instruction = ???
  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}VariableAssignment${variableCall.toString(nested + 1)}${newValue
      .toString(nested + 1)}"
  }
}
