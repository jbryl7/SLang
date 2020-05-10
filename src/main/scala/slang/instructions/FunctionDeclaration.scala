package slang.instructions

import slang.lexer.TokenType
import slang.lexer.TokenType.TokenType
import slang.utils
import slang.utils.ExceptionHandler

import scala.collection.mutable.ListBuffer

case class FunctionDeclaration(var identifier: String = "",
                               var returnType: TokenType = TokenType.Type,
                               var parameters: ListBuffer[Parameter] =
                                 ListBuffer(),
                               var body: Block = Block())
    extends Node
    with Instruction {

  override def execute(scope: Scope): Instruction = ???
  def setParentScope(scope: Option[Scope]) = body.setParentScope(scope)
  def setScope(scope: Scope) = body.setScope(scope)
  def getParameter(name: String): Option[Parameter] =
    parameters.find(_.name == name)
  def setParameters(params: ListBuffer[Parameter]) = { // todo - think of something that would solve types
    val noDeclarationOfVariableWithTheSameNameAsParam: Boolean = !params.forall(
      param =>
        body.scope.addVariable(
          new VarDeclaration(param.name, None, param.parameterType)))

    parameters = params
    noDeclarationOfVariableWithTheSameNameAsParam
  }

}
