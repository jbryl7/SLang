package slang.interpreter.runtimeclasses

import slang.lexer.Token
import slang.utils.{
  ExceptionHandler,
  MyRuntimeException,
  MyRuntimeExceptionType
}

import scala.collection.mutable

case class MyInstance(myClass: MyClass,
                      attributes: mutable.Map[String, Any] = mutable.Map()) {
  override def toString: String =
    f"${myClass.toString} object with attributes \n${attributes} \nand funs \n${myClass.functions}"

  def set(name: Token, value: Any): Unit = {
    if (attributes.keys.toList.contains(name.lexeme))
      attributes(name.lexeme) = value
    else
      ExceptionHandler.reportException(
        MyRuntimeException(MyRuntimeExceptionType.UndefinedAttribute),
        Some(f"Undefined property {name.toString}."))
  }

  def get(name: Token): Any = {
    if (attributes.keys.toList.contains(name.lexeme))
      attributes(name.lexeme)
    else if (myClass.get(name) != null) {

      myClass.get(name)
    } else {
      println(myClass.functions)
      ExceptionHandler.reportException(
        MyRuntimeException(MyRuntimeExceptionType.UndefinedAttribute),
        Some(s"Undefined property {name.toString}."))
    }
  }
}