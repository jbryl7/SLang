package slang.instructions

import slang.lexer.TokenType.TokenType

import scala.collection.mutable.ListBuffer

case class Condition(operands: ListBuffer[Node] = ListBuffer(),
                     operators: ListBuffer[TokenType] = ListBuffer())
    extends Expression {}
