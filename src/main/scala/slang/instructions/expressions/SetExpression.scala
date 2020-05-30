package slang.instructions.expressions

import slang.lexer.Token

case class SetExpression(val `object`: Expression,
                         val name: Token,
                         val value: Expression)
    extends Expression {
  override def accept[R](visitor: ExpressionVisitor[R]): R =
    visitor.visitSetExpr(this)

  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}SetExpression${`object`.toString(nested + 1)}${name
      .toString(nested + 1)}${value.toString(nested + 1)}"
  }
}
