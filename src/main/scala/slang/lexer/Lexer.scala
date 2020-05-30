package slang.lexer
import slang._
import slang.lexer.TokenType._
import slang.utils._
import scala.collection.mutable

case class Lexer(fileHandler: FileHandler) extends LexerInterface {
  def getNextToken: Option[Token] = {
    skipWhiteChars()
    val currentPosition = fileHandler.currentPosition.copy()

    fileHandler.consumeChar() match {
      case c
          if tokenTypeFromLexem(c.toString() + fileHandler.currentChar).isDefined =>
        val l = c.toString() + fileHandler.consumeChar
        Some(Token(tokenTypeFromLexem(l).get, l, currentPosition))

      case '/' =>
        if (fileHandler.currentChar == '/')
          skipCommentAndReturnToken()
        else
          Some(Token(TokenType.DivideOperator, '/', currentPosition))

      case c if tokenTypeFromLexem(c).isDefined =>
        Some(Token(tokenTypeFromLexem(c).get, c, currentPosition))

      case '"' =>
        getStringToken(currentPosition)

      case c if c.isDigit =>
        getNumberToken(c, currentPosition)

      case c if c.isLetter || c == '_' =>
        getIdentifierToken(c, currentPosition)

      case _ =>
        ExceptionHandler.reportException(
          LexerException(LexerExceptionType.InvalidSyntax),
          Some(messageWithPositionInFile(currentPosition = currentPosition)))
        None
    }
  }

  def skipWhiteChars(): Unit =
    while (fileHandler.currentChar.isWhitespace) fileHandler
      .consumeChar()

  def skipCommentAndReturnToken(): Option[Token] = {
    while (fileHandler.currentChar != '\n' && fileHandler.currentChar != EOF) fileHandler
      .consumeChar()
    getNextToken
  }
  def messageWithPositionInFile(lex: String = "",
                                currentPosition: CurrentPosition) =
    s"${currentPosition}  ${lex}"

  def getStringToken(position: CurrentPosition): Option[Token] = {
    var str: String = ""
    var previousChar = fileHandler.currentChar
    while (fileHandler.currentChar != '"' && fileHandler.currentChar != EOF) {
      previousChar = fileHandler.consumeChar()
      if (previousChar == '\\' && fileHandler.currentChar == '"') {
        str += previousChar
        previousChar = fileHandler.consumeChar()
      }
      str += previousChar
    }
    fileHandler.consumeChar()
    Some(Token(TokenType.StringLiteral, str, position))
  }

  def getIdentifierToken(c: Char, position: CurrentPosition): Option[Token] = {
    var lexem = c.toString()
    while (fileHandler.currentChar.isLetterOrDigit || fileHandler.currentChar == '_') lexem += fileHandler.consumeChar
    if (tokenTypeFromLexem(lexem).isDefined)
      return Some(Token(tokenTypeFromLexem(lexem).get, lexem, position))
    Some(Token(TokenType.Identifier, lexem, position))
  }

  def getNumberToken(c: Char, position: CurrentPosition): Option[Token] = {
    var retToken: Option[Token] = None
    var num: String = ""
    if (c == '0')
      retToken = Some(Token(TokenType.IntegerLiteral, c, position))
    else {
      num = c.toString
      while (fileHandler.currentChar.isDigit) num += fileHandler.consumeChar
      retToken = Some(Token(TokenType.IntegerLiteral, num, position))
    }
    if (fileHandler.currentChar.isLetter) {
      ExceptionHandler.reportException(
        LexerException(LexerExceptionType.IdentifierStartedWithDigit),
        Some(messageWithPositionInFile(num, position)))
    }
    retToken
  }

  val mapLexemToTokenType: mutable.Map[String, TokenType] = mutable.Map(
    ("&&", And),
    ("print", Print),
    (EOF.toString(), Eof),
    ("||", Or),
    ("true", True),
    ("false", False),
    ("Int", IntegerType),
    ("String", StringType),
    ("Unit", UnitType),
    ("else", Else),
    ("if", If),
    ("def", Fun),
    ("=", Assign),
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
    ("*", MultiplicativeOperator),
    ("+", Plus),
    ("-", Minus),
    ("/", DivideOperator),
    (",", Comma),
    (".", Dot),
    ("class", Class),
    ("return", Return)
  )

  def tokenTypeFromLexem(lexem: String): Option[TokenType] = {
    if (mapLexemToTokenType.contains(lexem))
      Some(mapLexemToTokenType(lexem))
    else
      None
  }
  implicit def charToString(c: Char): String = c.toString()
}
