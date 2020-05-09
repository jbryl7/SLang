package slang.instructions

import slang.lexer.TokenType
import slang.lexer.TokenType.TokenType

import scala.collection.mutable.ListBuffer

case class ClassDeclaration(
    identifier: String,
    classScope: Scope = Scope(),
    classType: TokenType = TokenType.Identifier) // type todo
    extends Node
    with Instruction {
  def addMethod(f: FunctionDeclaration): Boolean = classScope.addFunction(f)
  def addVariable(v: VarDeclaration): Boolean = classScope.addVariable(v)
  def isInScope(identifier: String): Boolean = classScope.isInScope(identifier)

  override def execute(scope: Scope): Instruction = ???
}
