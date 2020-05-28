package slang.instructions

import slang.lexer.TokenType.TokenType

trait Literal[T] extends Node {
  def myType(): TokenType
}
