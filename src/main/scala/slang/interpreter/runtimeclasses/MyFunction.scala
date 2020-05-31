package slang.interpreter.runtimeclasses

import slang.instructions.statements.FunctionStatement
import slang.interpreter.{Interpreter, Scope}
import slang.utils.{ExceptionHandler, MyRuntimeException}

import scala.collection.mutable.ListBuffer

case class MyFunction(declaration: FunctionStatement) extends MyCallable {
  override def call(visitor: Interpreter, args: ListBuffer[Any]): Any = {

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

  override def toString: String =
    f"${declaration.name.lexeme}(${declaration.params
      .map(p => f"${p.name.lexeme}: ${p.parameterType.lexeme}")
      .mkString(", ")})"
}
