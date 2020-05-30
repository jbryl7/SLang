package slang.interpreter

import slang.instructions.statements._
import slang.lexer.Token
import slang.utils.{
  ExceptionHandler,
  MyRuntimeException,
  MyRuntimeExceptionType
}

import scala.collection.mutable

case class Scope(functions: mutable.Map[String, Any] = mutable.Map(),
                 classes: mutable.Map[String, Any] = mutable.Map(),
                 vars: mutable.Map[String, Any] = mutable.Map(),
                 var parentScope: Scope = null) {

  def isInScope(identifier: String): Boolean =
    vars.keys.toList.contains(identifier)

  def setVariable(identifier: Token, value: Any): Unit = {
    if (isInScope(identifier.lexeme))
      vars(identifier.lexeme) = value
    else
      ExceptionHandler.reportException(
        MyRuntimeException(MyRuntimeExceptionType.UndefinedVariable),
        Some(identifier.toString))
  }
  def defineVariable(identifier: Token, value: Any): Unit = {
    if (!isInScope(identifier.lexeme))
      vars(identifier.lexeme) = value
    else
      ExceptionHandler.reportException(
        MyRuntimeException(MyRuntimeExceptionType.AlreadyDeclared),
        Some(identifier.toString))
  }

  def getVariable(identifier: Token): Any = {
    if (vars.keys.toList.contains(identifier.lexeme))
      vars(identifier.lexeme)
    else
      ExceptionHandler.reportException(
        MyRuntimeException(MyRuntimeExceptionType.UndefinedVariable),
        Some(identifier.toString))
  }

  def getFunctionsIdentifiers: List[String] =
    functions.keys.toList ++ parentScope.getFunctionsIdentifiers
  def getVariableIdentifiers: List[String] =
    vars.keys.toList ++ parentScope.getVariableIdentifiers
  def getClassesIdentifiers: List[String] =
    classes.keys.toList ++ parentScope.getClassesIdentifiers

}
