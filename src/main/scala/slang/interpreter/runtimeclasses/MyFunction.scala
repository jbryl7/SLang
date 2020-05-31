package slang.interpreter.runtimeclasses

import slang.instructions.statements.FunctionStatement
import slang.interpreter.{ExpressionVisitorImpl, Interpreter, Return, Scope}
import slang.utils.{ExceptionHandler, MyRuntimeException}

case class MyFunction(declaration: FunctionStatement) extends MyCallable {
  override def call(visitor: Interpreter, args: List[Any]): Any = {

    val funScope: Scope = Scope()
    declaration.params
      .zip(args)
      .foreach(param => funScope.define(param._1.name, param._2))
    funScope.parentScope = visitor.currentScope
    visitor.currentScope = funScope
    var ret: Any = ()

    try {
      visitor.execute(declaration.body)
    } catch { //add type checks
      case e: Return =>
        ret = e.value
      case e: MyRuntimeException => ExceptionHandler.reportException(e)
    }

    visitor.currentScope = visitor.currentScope.parentScope
    ret
  }

}
