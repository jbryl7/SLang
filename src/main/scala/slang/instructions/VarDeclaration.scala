package slang.instructions

import slang.lexer.{Token, TokenType}
import slang.lexer.TokenType.TokenType

import scala.collection.mutable

class VarDeclaration(identifier: String,
                     expression: Option[Node],
                     varType: TokenType,
                     vars: mutable.Map[String, VarDeclaration] = mutable.Map(),
                     functions: mutable.Map[String, FunctionDeclaration] =
                       mutable.Map(),
                     isObjectFromClass: Boolean = false)
    extends Node
    with Instruction {
  def getIdentifier: String = identifier
  def getExpression: Option[Node] = expression
  def getVarType: TokenType = varType

  def fromClassDeclaration(identifier: String,
                           classDeclaration: ClassDeclaration): VarDeclaration =
    new VarDeclaration(identifier,
                       None,
                       varType = classDeclaration.classType,
                       isObjectFromClass = true)

  def accessVar(variableCall: VariableCall): VarDeclaration =
    vars(variableCall.identifier)
  def accessFun(functionCall: FunctionCall): FunctionDeclaration =
    functions(functionCall.identifier)

  override def execute(scope: Scope): Instruction = this
  override def toString(): String =
    f"VarDeclaration(${identifier}, , ${varType}, ${expression})"
}
