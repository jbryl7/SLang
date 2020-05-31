package slang.instructions.statements

import slang.instructions.Node

trait Statement extends Node {
  def accept[R](visitor: StatementVisitor[R]): R
  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest} Statement"
  }
}
