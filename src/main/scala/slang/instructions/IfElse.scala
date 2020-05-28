package slang.instructions

case class IfElse(condition: Condition, block: Block, elseBlock: Option[Block])
    extends Node {

  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}IfElse\n${nest} condition${condition.toString(nested + 1)}\n${nest} ifBlock${block.toString(
      nested + 1)} \n${nest} elseBlock${elseBlock.map(_.toString(nested + 1)).getOrElse("")}"
  }
}
