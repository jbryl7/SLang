package slang.instructions

import slang.lexer.TokenType
import slang.lexer.TokenType.TokenType

case class MyInt() extends Literal[Int] {
  var value = 0
  def setVal(newVal: Int) = value = newVal
  def getVal() = value

  override def myType(): TokenType = TokenType.Number // todo
  override def execute(scope: Scope) = this
}
