package slang.instructions.expressions

case class LiteralExpr(var value: Any) extends Expr {
  override def accept[R](visitor: ExpressionVisitor[R]): R =
    visitor.visitLiteralExpr(this)

  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}LiteralExpression ${value.toString}"
  }
}
