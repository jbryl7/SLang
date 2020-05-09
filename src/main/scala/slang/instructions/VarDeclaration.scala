package slang.instructions

import slang.lexer.Token
import slang.lexer.TokenType.TokenType

case class VarDeclaration(identifier: String,
                          expression: Node,
                          varType: TokenType)
    extends Node
    with Instruction {
  override def execute(scope: Scope): Instruction = this
}
