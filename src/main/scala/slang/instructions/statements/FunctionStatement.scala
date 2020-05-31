package slang.instructions.statements

import slang.lexer.Token

import scala.collection.mutable.ListBuffer

case class FunctionStatement(name: Token,
                             returnType: Token,
                             params: ListBuffer[Parameter],
                             body: Block)
    extends Statement {

  override def accept[R](visitor: StatementVisitor[R]): R =
    visitor.visitFunctionStmt(this)

  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}Function\n ${nest}name${name}\n${nest} params:${params
      .map(_.toString(nested + 2))
      .mkString}${body.toString(nested + 1)}"
  }
}
