package slang.instructions

import slang.lexer.TokenType
import slang.lexer.TokenType.TokenType

case class MyInt(var value: Int = 0) extends Literal[Int] {
  def setVal(newVal: Int) = value = newVal
  def getVal() = value

  override def myType(): TokenType = TokenType.IntegerLiteral // todo
  override def execute(scope: Scope) = this
}
