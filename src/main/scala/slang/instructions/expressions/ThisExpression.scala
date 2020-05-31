package slang.instructions.expressions

import slang.lexer.Token

case class ThisExpression(val keyword: Token) extends Expression {
  override def accept[R](visitor: ExpressionVisitor[R]): R =
    visitor.visitThisExpression(this)

  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}ThisExpression${keyword.toString(nested + 1)}"
  }
}
