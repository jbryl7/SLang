package slang.instructions.expressions

import slang.lexer.Token

case class UnaryExpr(val operator: Token, val right: Expr) extends Expr {
  override def accept[R](visitor: ExpressionVisitor[R]): R =
    visitor.visitUnaryExpr(this)
  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}UnaryExpression${operator.toString(nested + 1)}${right
      .toString(nested + 1)}"
  }
}
