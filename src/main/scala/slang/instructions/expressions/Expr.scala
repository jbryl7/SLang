package slang.instructions.expressions

import slang.instructions.Node

trait Expr extends Node {
  def accept[R](visitor: ExpressionVisitor[R]): R
  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest} Expr"
  }
}
