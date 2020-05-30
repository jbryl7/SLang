package slang.instructions.expressions

import slang.lexer.Token

case class LogicalExpr(val left: Expr, val operator: Token, val right: Expr)
    extends Expr {
  override def accept[R](visitor: ExpressionVisitor[R]): R =
    visitor.visitLogicalExpr(this)

  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}LogicalExpression${operator.toString(nested + 1)}${left
      .toString(nested + 1)}${right.toString(nested + 1)}"
  }
}
