package slang.instructions.expressions

import slang.lexer.Token

case class VariableExpr(val name: Token) extends Expr {
  override def accept[R](visitor: ExpressionVisitor[R]): R =
    visitor.visitVariableExpr(this)
  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}VariableExpression${name.toString(nested + 1)}"
  }
}
