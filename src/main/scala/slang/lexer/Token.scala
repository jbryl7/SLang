package slang.lexer

import scala.collection.mutable
import TokenType.TokenType
import slang.EOF

case class Token(tokenType: TokenType,
                 lexeme: String,
                 position: CurrentPosition)

object TokenType extends Enumeration {
  type TokenType = Value

  val LeftParenthesis, RightParenthesis, LeftBrace, RightBrace, Comma, Dot,
  Minus, Plus, Semicolon, DivideOperator, MultiplicativeOperator, Bang,
  BangEqual, Equal, Assign, Greater, GreaterEqual, Less, LessEqual, Id,
  Identifier, StringLiteral, IntegerLiteral, And, Class, Else, False, True, Fun,
  Colon, For, If, NIL, Or, Print, Return, Super, This, Var, While, Type, Until,
  ForArrow, UnitType, StringType, IntegerType, Eof =
    Value

}
