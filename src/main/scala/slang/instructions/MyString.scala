package slang.instructions

import slang.lexer.TokenType
import slang.lexer.TokenType.TokenType

case class MyString(var value: String = "") extends Literal[String] {
  def setVal(newVal: String) = value = newVal
  def getVal(): String = value

  override def myType(): TokenType = TokenType.StringType // todo
  override def execute(scope: Scope): MyString = this
}
