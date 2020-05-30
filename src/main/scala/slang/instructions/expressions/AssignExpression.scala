package slang.instructions.expressions

import slang.lexer.Token

case class AssignExpression(name: Token, value: Expression) extends Expression {
  override def accept[R](visitor: ExpressionVisitor[R]): R =
    visitor.visitAssignExpr(this)

  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest} AssignExpression${name.toString(nested + 1)}${value
      .toString(nested + 1)}"
  }
}
