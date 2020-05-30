package slang.instructions.expressions

trait ExpressionVisitor[R] {
  def evaluate(expr: Expression) =
    expr.accept(this)
  def visitSetExpr(set: SetExpression): R
  def visitAssignExpr(expr: AssignExpression): R
  def visitBinaryExpr(expr: BinaryExpression): R
  def visitCallExpr(expr: CallExpression): R
  def visitGetExpr(expr: GetExpression): R
  def visitGroupingExpr(expr: GroupingExpression): R
  def visitLiteralExpr(expr: LiteralExpression): R
  def visitLogicalExpr(expr: LogicalExpression): R
  def visitThisExpr(expr: ThisExpression): R
  def visitUnaryExpr(expr: UnaryExpression): R
  def visitVariableExpr(expr: VariableExpression): R
}
