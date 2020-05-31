package slang.interpreter

import slang.instructions.expressions.Expression
import slang.instructions.statements._

trait StatementVisitorImpl
    extends StatementVisitor[Unit]
    with ExpressionVisitorImpl {

  override def visitBlockStmt(stmt: Block): Unit = {
    currentScope = Scope(parentScope = currentScope)
    stmt.statements.foreach(_.accept(this))
    currentScope = currentScope.parentScope
  }

  override def visitClassStmt(stmt: ClassStatement): Unit = ???

  override def visitExpressionStmt(stmt: ExpressionStatement): Unit = {
    println("visit expression stmt")
    evaluate(stmt.expression)
  }

  override def visitFunctionStmt(stmt: FunctionStatement): Unit = {
    currentScope.defineFunction(stmt)
  }

  override def visitIfStmt(stmt: IfStatement): Unit = {
    if (evaluate(stmt.condition).asInstanceOf[Boolean])
      execute(stmt.thenBlock)
    else if (stmt.elseBlock != null)
      execute(stmt.elseBlock)
  }

  override def visitPrintStmt(stmt: PrintStatement): Unit =
    println(evaluate(stmt.expression).toString)

  override def visitReturnStmt(stmt: ReturnStatement): Unit = {
    var value: Any = null
    if (stmt.value != null) {
      value = evaluate(stmt.value)
    }
    throw Return(value)
  }

  override def visitVarStmt(stmt: VarStatement): Unit = {
    var value: Any = null
    if (stmt.initializer != null)
      value = evaluate(stmt.initializer)
    //add typeChecks
    currentScope.defineVariable(stmt.name, value)
  }
}
