package slang.interpreter.runtimeclasses

import slang.instructions.statements.FunctionStatement
import slang.interpreter.{ExpressionVisitorImpl, Interpreter, Return, Scope}
import slang.utils.{ExceptionHandler, MyRuntimeException}

case class MyFunction(declaration: FunctionStatement) extends MyCallable {
  override def call(visitor: Interpreter, args: List[Any]): Any = {
    visitor.currentScope = Scope(parentScope = visitor.currentScope)
    declaration.params
      .zip(args)
      .foreach(param => visitor.currentScope.set(param._1.name, param._2))
    var ret: Any = null

    try { visitor.execute(declaration.body) } catch { //add type checks
      case e: Return =>
        ret = e.value
      case e: MyRuntimeException => ExceptionHandler.reportException(e)
    }

    visitor.currentScope = visitor.currentScope.parentScope
    ret
  }
}
