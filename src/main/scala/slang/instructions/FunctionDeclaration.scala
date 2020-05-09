package slang.instructions

import slang.lexer.TokenType
import slang.lexer.TokenType.TokenType

import scala.collection.mutable.ListBuffer

case class FunctionDeclaration(var identifier: String = "",
                               var returnType: TokenType = TokenType.Type,
                               var body: Block = Block(),
                               var parameters: ListBuffer[Parameter] =
                                 ListBuffer())
    extends Node
    with Instruction {

  override def execute(scope: Scope): Instruction = ???
  def setParentScope(scope: Scope) = body.setParentScope(scope)
  def setScope(scope: Scope) = body.setScope(scope)
  def getParameter(name: String): Option[Parameter] =
    parameters.find(_.name == name)
  def setParameters(params: ListBuffer[Parameter]) = parameters = params
}
