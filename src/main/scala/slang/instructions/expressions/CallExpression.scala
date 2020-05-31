package slang.instructions.expressions

import slang.lexer.Token

import scala.collection.mutable.ListBuffer

case class CallExpression(callee: Expression,
                          paren: Token,
                          arguments: ListBuffer[Expression])
    extends Expression {
  override def accept[R](visitor: ExpressionVisitor[R]): R =
    visitor.visitCallExpression(this)

  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}CallExpr${callee.toString(nested + 1)}${paren
      .toString(nested + 1)}${arguments.map(_.toString(nested + 1)).mkString}"
  }
}
