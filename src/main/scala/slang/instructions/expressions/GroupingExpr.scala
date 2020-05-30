package slang.instructions.expressions

case class GroupingExpr(val expression: Expr) extends Expr {
  override def accept[R](visitor: ExpressionVisitor[R]): R =
    visitor.visitGroupingExpr(this)
  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}GroupingExpression${expression.toString(nested + 1)}"
  }
}
