package slang.instructions.expressions

import slang.lexer.Token

case class LogicalExpression(val left: Expression,
                             val operator: Token,
                             val right: Expression)
    extends Expression {
  override def accept[R](visitor: ExpressionVisitor[R]): R =
    visitor.visitLogicalExpression(this)

  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}LogicalExpression${operator.toString(nested + 1)}${left
      .toString(nested + 1)}${right.toString(nested + 1)}"
  }
}
