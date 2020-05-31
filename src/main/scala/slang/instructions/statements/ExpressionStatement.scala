package slang.instructions.statements

import slang.instructions.expressions.Expression

case class ExpressionStatement(expression: Expression) extends Statement {
  override def accept[R](visitor: StatementVisitor[R]): R =
    visitor.visitExpressionStmt(this)
  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}Expression${expression.toString(nested + 1)}"
  }
}
