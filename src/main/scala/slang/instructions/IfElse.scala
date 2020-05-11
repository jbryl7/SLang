package slang.instructions

case class IfElse(condition: Condition, block: Block, elseBlock: Option[Block])
    extends Node
    with Instruction {
  override def execute(scope: Scope): Instruction = {
    if (condition.execute(scope))
      block.execute(scope)
    elseBlock.get.execute(scope)
  }

  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}IfElse\n${nest} condition${condition.toString(nested + 1)}\n${nest} ifBlock${block.toString(
      nested + 1)} \n${nest} elseBlock${elseBlock.map(_.toString(nested + 1)).getOrElse("")}"
  }
}
