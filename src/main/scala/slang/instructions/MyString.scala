package slang.instructions

import slang.lexer.TokenType
import slang.lexer.TokenType.TokenType

case class MyString(var value: String = "") extends Literal[String] {
  def setVal(newVal: String) = value = newVal
  def getVal(): String = value

  override def myType(): TokenType = TokenType.StringType // todo

  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}MyString\n${nest}value: ${value}\n${nest}myType: ${myType()}"
  }
}
