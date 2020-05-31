package slang.instructions.expressions

import slang.lexer.Token

case class SetExpression(val obj: Expression,
                         val name: Token,
                         val value: Expression)
    extends Expression {
  override def accept[R](visitor: ExpressionVisitor[R]): R =
    visitor.visitSetExpression(this)

  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}SetExpression${obj.toString(nested + 1)}${name
      .toString(nested + 1)}${value.toString(nested + 1)}"
  }
}
