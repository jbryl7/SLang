package slang.interpreter

import slang.instructions.expressions._
import slang.instructions.statements._
import slang.parser.Parser
case class Interpreter(parser: Parser)
    extends ExpressionVisitor[Any]
    with StatementVisitor[Unit] {
  var currentScope = Scope()

  def interpret() = {
    val rootBlock = parser.parse()
    rootBlock.statements.foreach(execute)
  }

  override def visitBlockStmt(stmt: Block) = ???

  override def visitClassStmt(stmt: ClassStatement) = ???

  override def visitExpressionStmt(stmt: ExpressionStatement) =
    ???

  override def visitFunctionStmt(stmt: FunctionStatement) = ???

  override def visitIfStmt(stmt: IfStatement) = ???

  override def visitPrintStmt(stmt: PrintStatement) = ???

  override def visitReturnStmt(stmt: ReturnStatement) = ???

  override def visitVarStmt(stmt: VarStatement) = ???

  override def visitSetExpr(set: SetExpr) = ???

  override def visitAssignExpr(expr: AssignExpr) = ???

  override def visitBinaryExpr(expr: BinaryExpr) = ???

  override def visitCallExpr(expr: CallExpr) = ???

  override def visitGetExpr(expr: GetExpr) = ???

  override def visitGroupingExpr(expr: GroupingExpr) = ???

  override def visitLiteralExpr(expr: LiteralExpr) = ???

  override def visitLogicalExpr(expr: LogicalExpr) = ???

  override def visitThisExpr(expr: ThisExpr) = ???

  override def visitUnaryExpr(expr: UnaryExpr) = ???

  override def visitVariableExpr(expr: VariableExpr) = ???
}
