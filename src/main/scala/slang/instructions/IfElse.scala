package slang.instructions

case class IfElse(condition: Condition, block: Block, elseBlock: Option[Block])
    extends Node
    with Instruction {
  override def execute(scope: Scope): Instruction = {
    if (condition.execute(scope))
      block.execute(scope)
    elseBlock.get.execute(scope)
  }
}
