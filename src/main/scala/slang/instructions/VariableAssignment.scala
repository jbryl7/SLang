package slang.instructions

case class VariableAssignment(variableCall: VariableCall, newValue: Node)
    extends Node {
  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}VariableAssignment${variableCall.toString(nested + 1)}${newValue
      .toString(nested + 1)}"
  }
}
