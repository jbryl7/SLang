package slang.instructions.statements

import slang.instructions.expressions.Expr

case class ExpressionStatement(expression: Expr) extends Statement {
  override def accept[R](visitor: StatementVisitor[R]): R =
    visitor.visitExpressionStmt(this)
  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}Expression${expression.toString(nested + 1)}"
  }
}
