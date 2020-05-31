package slang.interpreter.runtimeclasses

import slang.lexer.Token
import slang.utils.{
  ExceptionHandler,
  MyRuntimeException,
  MyRuntimeExceptionType
}

class MyClass(name: String, functions: Map[String, MyFunction])
    extends MyCallable {
  override def toString: String =
    f"${name}"

  def get(name: Token): Any = {
    if (functions.keys.toList.contains(name.lexeme))
      functions.get(name.lexeme)
    else
      ExceptionHandler.reportException(
        MyRuntimeException(MyRuntimeExceptionType.UndefinedAttribute),
        Some(
          f"Undefined method ${name.lexeme} in class ${name}. ${name.position}"))
  }
}
