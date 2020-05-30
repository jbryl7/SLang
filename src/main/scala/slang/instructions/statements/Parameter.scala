package slang.instructions.statements

import slang.instructions.Node
import slang.lexer.Token

case class Parameter(name: Token, parameterType: Token) extends Node {
  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}Param\n${nest} name: ${name} \n${nest} type ${parameterType}"
  }
}
