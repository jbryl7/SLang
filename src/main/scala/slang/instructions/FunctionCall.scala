package slang.instructions

import slang.interpreter.Scope
import slang.lexer.TokenType
import slang.lexer.TokenType.TokenType
import slang.utils._

import scala.collection.mutable.ListBuffer
import util.control.Breaks._

case class FunctionCall(identifier: String,
                        var arguments: ListBuffer[Node] = ListBuffer(),
                        maybeAttributeCall: Option[Node] = None)
    extends Node {

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

  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}FunctionCall\n${nest} identifier ${identifier}\n${nest} maybeAttributeCall ${maybeAttributeCall
      .map(_.toString(nested + 1))
      .getOrElse(" None")} \n${nest} arguments: ${arguments
      .map(_.toString(nested + 1))
      .mkString}\n${nest}"
  }
}
