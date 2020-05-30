package slang.instructions

import slang.lexer.Token
import slang.lexer.TokenType.TokenType

case class Parameter(name: Token, parameterType: Token) extends Node {
  def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}Param\n${nest} name: ${name} \n${nest} type ${parameterType}"
  }
}
