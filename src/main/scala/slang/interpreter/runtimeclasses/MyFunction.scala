package slang.interpreter.runtimeclasses

import slang.instructions.statements.FunctionStatement
import slang.interpreter.{Interpreter, Scope}
import slang.utils.{
  ExceptionHandler,
  MyRuntimeException,
  MyRuntimeExceptionType
}

import scala.collection.mutable.ListBuffer

case class MyFunction(declaration: FunctionStatement) extends MyCallable {
  override def call(visitor: Interpreter, args: ListBuffer[Any]): Any = {
    if (args.length != declaration.params.length)
      ExceptionHandler.reportException(
        MyRuntimeException(MyRuntimeExceptionType.InvalidNumberOfArguments),
        Some("when called " + declaration.name.lexeme))

    val funScope: Scope = Scope()
    declaration.params
      .zip(args)
      .foreach(param => funScope.define(param._1.name, param._2))

    var ret: Any = ()
    funScope.parentScope = visitor.currentScope

    try {
      visitor.executeBlock(declaration.body, funScope)
    } catch { //add type checks
      case e: Return =>
        ret = e.value
      case e: MyRuntimeException => ExceptionHandler.reportException(e)
    }
    ret
  }

  override def toString: String =
    f"${declaration.name.lexeme}(${declaration.params
      .map(p => f"${p.name.lexeme}: ${p.parameterType.lexeme}")
      .mkString(", ")})"
}
