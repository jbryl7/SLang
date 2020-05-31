package slang.interpreter

import slang.instructions.statements._
import slang.interpreter.runtimeclasses.MyFunction
import slang.lexer.Token
import slang.utils.{
  ExceptionHandler,
  MyRuntimeException,
  MyRuntimeExceptionType
}

import scala.collection.mutable

case class Scope(functions: mutable.Map[String, MyFunction] = mutable.Map(),
                 classes: mutable.Map[String, Any] = mutable.Map(),
                 vars: mutable.Map[String, Any] = mutable.Map(),
                 var parentScope: Scope = null) {

  def isVarInScope(identifier: String): Boolean =
    vars.keys.toList.contains(identifier)
  def isFunInScope(identifier: String): Boolean =
    functions.keys.toList.contains(identifier)

  def setVariable(identifier: Token, value: Any): Unit = {
    if (isVarInScope(identifier.lexeme))
      vars(identifier.lexeme) = value
    else if (parentScope != null)
      parentScope.setVariable(identifier, value)
    else
      ExceptionHandler.reportException(
        MyRuntimeException(MyRuntimeExceptionType.UndefinedVariable),
        Some(identifier.toString))
  }
  def defineVariable(identifier: Token, value: Any): Unit = {
    if (!isVarInScope(identifier.lexeme))
      vars(identifier.lexeme) = value
    else
      ExceptionHandler.reportException(
        MyRuntimeException(MyRuntimeExceptionType.AlreadyDeclared),
        Some(identifier.toString))
  }
  def getVariable(identifier: Token): Any = {
    if (vars.keys.toList.contains(identifier.lexeme))
      vars(identifier.lexeme)
    else if (parentScope != null)
      parentScope.getVariable(identifier)
    else {
      ExceptionHandler.reportException(
        MyRuntimeException(MyRuntimeExceptionType.UndefinedVariable),
        Some(identifier.toString))
      null
    }
  }
  def defineFunction(function: FunctionStatement): Unit = {
    if (!isFunInScope(function.name.lexeme))
      functions(function.name.lexeme) = MyFunction(function, null)
    else {
      ExceptionHandler.reportException(
        MyRuntimeException(MyRuntimeExceptionType.AlreadyDeclared),
        Some(function.name.toString))
    }
  }
  def getFunction(identifier: Token): Any = {
    if (isFunInScope(identifier.lexeme)) {
      print("getting fun")
      functions(identifier.lexeme)
      print("success fun")
    } else if (parentScope != null)
      parentScope.getFunction(identifier)
    else {
      ExceptionHandler.reportException(
        MyRuntimeException(MyRuntimeExceptionType.UndefinedVariable),
        Some(identifier.toString))
      null
    }
  }
  def getFunctionsIdentifiers: List[String] =
    functions.keys.toList ++ parentScope.getFunctionsIdentifiers
  def getVariableIdentifiers: List[String] =
    vars.keys.toList ++ parentScope.getVariableIdentifiers
  def getClassesIdentifiers: List[String] =
    classes.keys.toList ++ parentScope.getClassesIdentifiers

}
