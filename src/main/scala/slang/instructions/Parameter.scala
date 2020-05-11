package slang.instructions

import slang.lexer.TokenType.TokenType

case class Parameter(name: String, parameterType: TokenType) {
  def toString(nested: Int): String = {
    var nest = " "
    for (x <- 0 to nested)
      nest += "  "
    f"\n${nest}Param\n${nest} name: ${name} \n${nest} type ${parameterType}"
  }
}
