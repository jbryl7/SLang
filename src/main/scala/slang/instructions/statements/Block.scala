package slang.instructions.statements

import scala.collection.mutable.ListBuffer

case class Block(statements: ListBuffer[Statement]) extends Statement {
  override def accept[R](visitor: StatementVisitor[R]): R =
    visitor.visitBlockStmt(this)
  override def toString: String = toString(0)
  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}Block\n ${nest}statements:${statements.map(_.toString(nested + 2)).mkString}"
  }
}
