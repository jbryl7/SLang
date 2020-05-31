package slang.instructions.expressions

import slang.lexer.Token

case class UnaryExpression(val operator: Token, val right: Expression)
    extends Expression {
  override def accept[R](visitor: ExpressionVisitor[R]): R =
    visitor.visitUnaryExpression(this)
  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}UnaryExpression${operator.toString(nested + 1)}${right
      .toString(nested + 1)}"
  }
}
