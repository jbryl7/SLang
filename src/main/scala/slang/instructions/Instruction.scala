package slang.instructions

trait Instruction {
  def execute(scope: Scope): Instruction
}
