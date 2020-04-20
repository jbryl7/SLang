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
  Minus, Plus, Semicolon, Slash, Star, Bang, BangEqual, Equal, Assign, Greater,
  GreaterEqual, Less, LessEqual, Id, Identifier, String, Number, And, Class,
  Else, False, True, Fun, Colon, For, If, NIL, Or, Print, Return, Super, This,
  Var, While, Type, Until, ForArrow, Eof = Value

  val mapLexemToTokenType: mutable.Map[String, TokenType] = mutable.Map(
    ("&&", And),
    (EOF.toString(), Eof),
    ("||", Or),
    ("Int", Type),
    ("else", Else),
    ("if", If),
    ("def", Fun),
    ("=", Assign),
    ("for", For),
    ("until", Until),
    ("<-", ForArrow),
    ("var", Var),
    (">", Greater),
    ("<", Less),
    ("<=", LessEqual),
    (">=", GreaterEqual),
    ("==", Equal),
    ("!", Bang),
    (":", Colon),
    ("!=", BangEqual),
    ("(", LeftParenthesis),
    (")", RightParenthesis),
    ("}", RightBrace),
    ("{", LeftBrace),
    ("*", Star),
    ("+", Plus),
    ("-", Minus),
    ("/", Slash),
    (",", Comma),
    (".", Dot),
    ("Nil", NIL),
    ("class", Class),
    ("return", Return)
  )

  def fromLexem(lexem: String): Option[TokenType] = {
    if (mapLexemToTokenType.contains(lexem))
      Some(mapLexemToTokenType(lexem))
    else
      None
  }

}
