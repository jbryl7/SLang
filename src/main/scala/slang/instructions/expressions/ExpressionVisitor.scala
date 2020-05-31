package slang.instructions.expressions

trait ExpressionVisitor[R] {
  def evaluate(expr: Expression) =
    expr.accept(this)
  def visitSetExpression(set: SetExpression): R
  def visitAssignExpression(expr: AssignExpression): R
  def visitBinaryExpression(expr: BinaryExpression): R
  def visitCallExpression(expr: CallExpression): R
  def visitGetExpression(expr: GetExpression): R
  def visitGroupingExpression(expr: ParenthExpression): R
  def visitLiteralExpression(expr: LiteralExpression): R
  def visitLogicalExpression(expr: LogicalExpression): R
  def visitThisExpression(expr: ThisExpression): R
  def visitUnaryExpression(expr: UnaryExpression): R
  def visitVariableExpression(expr: VariableExpression): R
}
