package slang.instructions

import slang.lexer.{Token, TokenType}
import slang.lexer.TokenType.TokenType

import scala.collection.mutable

class VarDeclaration(identifier: String,
                     expression: Option[Node],
                     varType: TokenType)
    extends Node {
  def getIdentifier: String = identifier
  def getExpression: Option[Node] = expression
  def getVarType: TokenType = varType

  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}VarDeclaration\n${nest} varType ${varType}\n${nest} expression${expression
      .map(_.toString(nested + 1))
      .getOrElse("")}\n${nest}\n${nest}"
  }
}
