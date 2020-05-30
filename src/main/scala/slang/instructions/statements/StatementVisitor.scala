package slang.instructions.statements

trait StatementVisitor[R] {

  def execute(statement: Statement) =
    statement.accept(this)

  def visitBlockStmt(stmt: Block): R
  def visitClassStmt(stmt: ClassStatement): R
  def visitExpressionStmt(stmt: ExpressionStatement): R
  def visitFunctionStmt(stmt: FunctionStatement): R
  def visitIfStmt(stmt: IfStatement): R
  def visitPrintStmt(stmt: PrintStatement): R
  def visitReturnStmt(stmt: ReturnStatement): R
  def visitVarStmt(stmt: VarStatement): R
}
