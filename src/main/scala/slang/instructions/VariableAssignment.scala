package slang.instructions

case class VariableAssignment(variableCall: VariableCall, newValue: Node)
    extends Instruction
    with Node {
  override def execute(scope: Scope): Instruction = ???
}
