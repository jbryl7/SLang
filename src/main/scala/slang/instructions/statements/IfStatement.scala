package slang.instructions.statements

import slang.instructions.expressions.Expr

case class IfStatement(condition: Expr,
                       thenBlock: Statement,
                       elseBlock: Statement)
    extends Statement {

  override def accept[R](visitor: StatementVisitor[R]): R =
    visitor.visitIfStmt(this)

  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}IfStatement\n${nest} condition${condition.toString(nested + 2)}\n${nest} thenBlock${thenBlock
      .toString(nested + 2)}\n${nest} elseBlock${elseBlock.toString(nested + 2)}"
  }
}
