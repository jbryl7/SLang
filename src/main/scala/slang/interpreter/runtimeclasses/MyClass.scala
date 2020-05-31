package slang.interpreter.runtimeclasses

import slang.instructions.statements.Parameter
import slang.interpreter.Interpreter
import slang.lexer.Token
import slang.utils._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
case class MyClass(name: String,
                   params: ListBuffer[Parameter] = ListBuffer(),
                   functions: mutable.Map[String, MyFunction] = mutable.Map())
    extends MyCallable {
  override def toString: String =
    f"${name}"

  def get(name: Token): Any = {
    if (functions.keys.toList.contains(name.lexeme))
      functions(name.lexeme)
    else
      null
  }
  override def call(visitor: Interpreter, args: ListBuffer[Any]): Any = {
    if (args.length != params.toList.length)
      ExceptionHandler.reportException(
        MyRuntimeException(MyRuntimeExceptionType.InvalidNumberOfArguments),
        Some(this.toString))
    val instanceAttributes: mutable.Map[String, Any] = mutable.Map()
    args.zip(params).foreach(p => instanceAttributes(p._2.name.lexeme) = p._1)
    MyInstance(this, instanceAttributes)
  }
}
