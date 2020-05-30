package slang.instructions.statements
import slang.instructions.expressions.Expr
import slang.lexer.Token

case class VarStatement(name: Token, initializer: Expr, varType: Token)
    extends Statement {

  override def accept[R](visitor: StatementVisitor[R]): R =
    visitor.visitVarStmt(this)

  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}Var\n ${nest}name:${name.toString(nested + 2)} \n${nest} initializer:${initializer
      .toString(nested + 2)}\n${nest} varType: ${varType.toString(nested + 2)}"
  }
}
