package slang.interpreter.runtimeclasses

import slang.lexer.Token
import slang.utils.{
  ExceptionHandler,
  MyRuntimeException,
  MyRuntimeExceptionType
}

case class MyInstance(myClass: MyClass, attributes: Map[String, Any]) {
  override def toString: String =
    f"${myClass.toString} object ${attributes}"

  def get(name: Token): Any = {
    if (attributes.keys.toList.contains(name.lexeme))
      attributes.get(name.lexeme)
    else
      ExceptionHandler.reportException(
        MyRuntimeException(MyRuntimeExceptionType.UndefinedAttribute),
        Some("Undefined property '" + name.lexeme + "'."))
  }
}
