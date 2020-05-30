package slang.instructions.statements

import slang.instructions.Node
import slang.lexer.Token

import scala.collection.mutable.ListBuffer

case class ClassStatement(name: Token, classBody: ClassBody) extends Statement {

  override def accept[R](visitor: StatementVisitor[R]): R =
    visitor.visitClassStmt(this)
  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}Class\n ${nest}name${name}\n ${nest}classBody${classBody
      .toString(nested + 1)}"
  }
}

case class ClassBody(vars: ListBuffer[VarStatement] = ListBuffer(),
                     funs: ListBuffer[FunctionStatement] = ListBuffer())
    extends Node {

  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest} classBody \n${nest} vars:${vars
      .map(_.toString(nested + 1))
      .mkString("")}\n ${nest}functions: ${funs.map(_.toString(nested + 1)).mkString("")}"
  }
}
