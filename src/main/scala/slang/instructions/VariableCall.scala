package slang.instructions

case class VariableCall(name: String) extends Instruction with Node {
  override def execute(scope: Scope): Instruction = this
}
