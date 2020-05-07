package slang.instructions

import slang.lexer.TokenType
import slang.lexer.TokenType.TokenType
import slang.utils._

import scala.collection.mutable.ListBuffer
import util.control.Breaks._

case class FunctionCall(name: String,
                        var arguments: ListBuffer[Node] = ListBuffer())
    extends Node
    with Instruction {

  def execute(scope: Scope) = ???

  def getFunctionFromScope(scope: Scope): Option[FunctionDeclaration] = {
    var tmpScope = scope
    var maybeFun = Option.empty[FunctionDeclaration]
    breakable {
      while (maybeFun.isEmpty) {
        if (scope.isInScope(name))
          maybeFun = Some(scope.functions(name))
        else
          tmpScope = tmpScope.parentScope
        if (tmpScope == null) break
      }
    }
    maybeFun
  }

  def checkParameters(fun: FunctionDeclaration): Unit = {
    if (fun.parameters.size != arguments.size)
      ExceptionHandler.reportException(
        RuntimeException(RuntimeExceptionType.InvalidNumberOfArguments))
    fun.parameters
      .zip(arguments)
      .foreach(pair => checkParameter(pair._1, TokenType.Type))
  }
  def checkParameter(parameter: Parameter, givenType: TokenType): Unit = {
    if (parameter.parameterType != givenType)
      ExceptionHandler.reportException(
        RuntimeException(RuntimeExceptionType.InvalidArgumentException))
  }
}
