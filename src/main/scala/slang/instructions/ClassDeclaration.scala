package slang.instructions

import slang.lexer.TokenType
import slang.lexer.TokenType.TokenType

import scala.collection.mutable.ListBuffer

case class ClassDeclaration(
    identifier: String,
    classBody: ClassBody,
    classType: TokenType = TokenType.Identifier) // type todo
    extends Node {

  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}ClassDeclaration\n${nest} identifier ${identifier}\n${nest} classType: ${classType}\n${nest} ${classBody
      .toString(nested + 1)}"
  }
}
