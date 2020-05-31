package slang.lexer

import scala.collection.mutable
import TokenType.TokenType
import slang.EOF

case class Token(tokenType: TokenType,
                 lexeme: String,
                 position: CurrentPosition) {
  def getNest(nested: Int): String = {
    var nest = ""
    for (x <- 0 to nested)
      nest += "  "
    nest
  }
  def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}Token\n${nest} type: ${tokenType}\n${nest} lexeme: ${lexeme}\n${nest} pos: ${position}"
  }
}
object TokenType extends Enumeration {
  type TokenType = Value

  val LeftParenthesis, RightParenthesis, LeftBrace, RightBrace, Comma, Dot,
  Minus, Plus, Semicolon, DivideOperator, MultiplicativeOperator, Bang,
  BangEqual, Equal, Assign, Greater, GreaterEqual, Less, LessEqual, Id,
  Identifier, StringLiteral, IntegerLiteral, And, Class, Else, False, True, Fun,
  Colon, If, Or, Print, Return, This, Var, Type, UnitType, StringType,
  IntegerType, Nil, Eof =
    Value

}
