package slang.instructions.expressions

case class LiteralExpression(var value: Any) extends Expression {
  override def accept[R](visitor: ExpressionVisitor[R]): R =
    visitor.visitLiteralExpression(this)

  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}LiteralExpression ${value.toString}"
  }
}
