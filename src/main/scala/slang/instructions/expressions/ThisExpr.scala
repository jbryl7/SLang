package slang.instructions.expressions

import slang.lexer.Token

case class ThisExpr(val keyword: Token) extends Expr {
  override def accept[R](visitor: ExpressionVisitor[R]): R =
    visitor.visitThisExpr(this)

  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}ThisExpression${keyword.toString(nested + 1)}"
  }
}
