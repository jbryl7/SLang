package slang.instructions.expressions

import slang.lexer.Token

case class GetExpression(val obj: Expression, val name: Token)
    extends Expression {
  override def accept[R](visitor: ExpressionVisitor[R]): R =
    visitor.visitGetExpression(this)

  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}GetExpression${obj.toString(nested + 1)}${name
      .toString(nested + 1)}"
  }
}
