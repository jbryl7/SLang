package slang.instructions.expressions

import slang.lexer.Token

case class BinaryExpr(left: Expr, operator: Token, right: Expr) extends Expr {
  override def accept[R](visitor: ExpressionVisitor[R]): R =
    visitor.visitBinaryExpr(this)

  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest} BinaryExpression${operator.toString(nested + 1)}${left
      .toString(nested + 1)}${right.toString(nested + 1)}"
  }
}
