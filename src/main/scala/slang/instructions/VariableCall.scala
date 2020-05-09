package slang.instructions

case class VariableCall(identifier: String) extends Instruction with Node {
  override def execute(scope: Scope): Instruction = this
}
