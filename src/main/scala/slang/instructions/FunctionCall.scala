package slang.instructions

import slang.lexer.TokenType
import slang.lexer.TokenType.TokenType
import slang.utils._

import scala.collection.mutable.ListBuffer
import util.control.Breaks._

case class FunctionCall(identifier: String,
                        var arguments: ListBuffer[Node] = ListBuffer())
    extends Node
    with Instruction {

  def execute(scope: Scope) = ???

  def getFunctionFromScope(scope: Scope): Option[FunctionDeclaration] =
    scope.getFunctionDeclaration(identifier)

  def checkParameters(fun: FunctionDeclaration): Unit = {
    if (fun.parameters.size != arguments.size)
      ExceptionHandler.reportException(
        MyRuntimeException(MyRuntimeExceptionType.InvalidNumberOfArguments))
    fun.parameters
      .zip(arguments)
      .foreach(pair => checkParameter(pair._1, TokenType.Type))
  }
  def checkParameter(parameter: Parameter, givenType: TokenType): Unit = {
    if (parameter.parameterType != givenType)
      ExceptionHandler.reportException(
        MyRuntimeException(MyRuntimeExceptionType.InvalidArgumentException))
  }
}
