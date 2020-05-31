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
    f"${myClass.toString}(${attributes.toList.reverse.map(_._2).mkString(", ")})"

  def set(name: Token, value: Any): Unit = {
    if (attributes.keys.toList.contains(name.lexeme))
      attributes(name.lexeme) = value
    else
      ExceptionHandler.reportException(
        MyRuntimeException(MyRuntimeExceptionType.UndefinedAttribute),
        Some(
          f"Undefined property ${name.lexeme} in object of class ${myClass} ${name.position}.")
      )
  }

  def get(name: Token): Any = {
    if (attributes.keys.toList.contains(name.lexeme))
      attributes(name.lexeme)
    else if (myClass.get(name) != null) {
      val fun = myClass.get(name)
      fun.copy(binding = this)
    } else {
      println(myClass.functions)
      ExceptionHandler.reportException(
        MyRuntimeException(MyRuntimeExceptionType.UndefinedAttribute),
        Some(
          s"Undefined property ${name.lexeme} in object of class ${myClass} ${name.position}.")
      )
    }
  }
}
