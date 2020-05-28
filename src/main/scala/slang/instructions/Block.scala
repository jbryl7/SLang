package slang.instructions

import scala.collection.mutable.ListBuffer

case class Block(instructions: ListBuffer[Node] = ListBuffer()) extends Node {
  def addInstruction(instruction: Node) = instructions.append(instruction)
  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}Block:\n${nest} instructions: ${instructions.map(_.toString(nested + 1)).mkString("")}"
  }
}
