package slang.instructions

import slang.lexer.TokenType.TokenType

trait Literal[T] extends Instruction with Node {
  def myType(): TokenType
}
