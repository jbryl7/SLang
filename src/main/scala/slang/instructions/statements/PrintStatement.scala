package slang.instructions.statements

import slang.instructions.expressions.Expression

case class PrintStatement(expression: Expression) extends Statement {

  override def accept[R](visitor: StatementVisitor[R]): R =
    visitor.visitPrintStmt(this)

  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}Print${expression.toString(nested + 1)}"
  }
}
