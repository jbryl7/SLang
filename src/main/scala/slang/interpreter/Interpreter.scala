package slang.interpreter

import slang.instructions._
import slang.utils.{ExceptionHandler, MyRuntimeException, MyRuntimeExceptionType}

case class Interpreter(rootBlock: Block) {
  var currentScope = Scope()

  def interpret(): Int = {
    rootBlock.instructions.foreach(visit(_))
    0
  }
  def visit(node: Node): Int
  def visit(functionDeclaration: FunctionDeclaration): Int = {
    currentScope.addFunction(functionDeclaration)
    0
  }
  def visit(varDeclaration: VarDeclaration): Int = {
    currentScope.addVariable(varDeclaration)
    0
  }
  def visit(classDeclaration: ClassDeclaration): Int = {
    currentScope.addClass(classDeclaration)
    0
  }
  def visit(functionCall: FunctionCall): Int = {
    val maybeFunDeclaration = currentScope.getFunctionDeclaration(functionCall.identifier)
    if (maybeFunDeclaration.isDefined) {
      //todo

    } else
      ExceptionHandler.reportException(MyRuntimeException(MyRuntimeExceptionType.IdentifierNotInScope), Some(functionCall.toString))
    0
  }
  def visit(variableCall: VariableCall): Int = {
    val maybeVariable = currentScope.getVariableDeclaration(variableCall.identifier)
    if (maybeVariable.isDefined) {

    } else
      ExceptionHandler.reportException(MyRuntimeException(MyRuntimeExceptionType.IdentifierNotInScope), Some(variableCall.toString))
    0
  }
  def visit(variableAssignment: VariableAssignment): Int = {
    currentScope.getVariableDeclaration(variableAssignment.variableCall.identifier)
    //todo
    0
  }
  def visit(ifElse: IfElse): Int = {
    if (visit(ifElse.condition) == 1)
      visit(ifElse.block)
    else if (ifElse.elseBlock.isDefined)
      visit(ifElse.elseBlock.get)
    else
      0
  }
  def visit(expression: Expression): Int
  def visit(condition: Condition): Int
  def visit(block: Block): Int = {
    block.instructions.foreach(visit(_))
    0
  }



  def addScope() = {
    val newScope = Scope()
    newScope.parentScope = Some(currentScope)
    currentScope = newScope
  }
  def destroyCurrentScope() =
    currentScope = currentScope.parentScope.getOrElse(Scope())


}
