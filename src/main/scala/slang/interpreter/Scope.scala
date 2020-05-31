package slang.interpreter

import slang.lexer.Token
import slang.utils._

import scala.collection.mutable

case class Scope(vars: mutable.Map[String, Any] = mutable.Map(),
                 var parentScope: Scope = null) {

  def isInScope(identifier: String): Boolean =
    vars.keys.toList.contains(identifier)

  def set(identifier: Token, value: Any): Unit = {
    if (isInScope(identifier.lexeme))
      vars(identifier.lexeme) = value
    else if (parentScope != null)
      parentScope.set(identifier, value)
    else
      ExceptionHandler.reportException(
        MyRuntimeException(MyRuntimeExceptionType.UndefinedVariable),
        Some(identifier.toString))
  }
  def define(identifier: Token, value: Any): Unit = {
    vars(identifier.lexeme) = value
  }
  def get(identifier: Token): Any = {
    if (vars.keys.toList.contains(identifier.lexeme))
      vars(identifier.lexeme)
    else if (parentScope != null)
      parentScope.get(identifier)
    else {
      ExceptionHandler.reportException(
        MyRuntimeException(MyRuntimeExceptionType.UndefinedVariable),
        Some(identifier.toString))
      null
    }
  }

  def getVariableIdentifiers: List[String] =
    vars.keys.toList ++ parentScope.getVariableIdentifiers

}
