package slang.instructions.expressions

import slang.lexer.Token

case class GetExpression(val `object`: Expression, val name: Token)
    extends Expression {
  override def accept[R](visitor: ExpressionVisitor[R]): R =
    visitor.visitGetExpr(this)

  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}GetExpression${`object`.toString(nested + 1)}${name
      .toString(nested + 1)}"
  }
}
