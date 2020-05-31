package slang.instructions.statements

import slang.instructions.expressions.Expression
import slang.lexer.Token

case class ReturnStatement(keyword: Token, value: Expression)
    extends Statement {

  override def accept[R](visitor: StatementVisitor[R]): R =
    visitor.visitReturnStmt(this)

  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}Return\n ${nest}keyword${keyword.toString(nested + 1)}\n${nest} value${value
      .toString(nested + 1)}"
  }
}
