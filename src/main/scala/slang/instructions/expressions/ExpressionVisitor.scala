package slang.instructions.expressions

trait ExpressionVisitor[R] {
  def evaluate(expr: Expr) =
    expr.accept(this)
  def visitSetExpr(set: SetExpr): R
  def visitAssignExpr(expr: AssignExpr): R
  def visitBinaryExpr(expr: BinaryExpr): R
  def visitCallExpr(expr: CallExpr): R
  def visitGetExpr(expr: GetExpr): R
  def visitGroupingExpr(expr: GroupingExpr): R
  def visitLiteralExpr(expr: LiteralExpr): R
  def visitLogicalExpr(expr: LogicalExpr): R
  def visitThisExpr(expr: ThisExpr): R
  def visitUnaryExpr(expr: UnaryExpr): R
  def visitVariableExpr(expr: VariableExpr): R
}
