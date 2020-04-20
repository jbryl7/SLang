package slang.lexer
import slang._
import slang.utils.{
  AnalysePhase,
  ExceptionHandler,
  LexerException,
  LexerExceptionType
}

case class Lexer(fileHandler: FileHandler) {
  def getNextToken: Option[Token] = {
    while (fileHandler.currentChar.isWhitespace || fileHandler.currentChar == '\n') fileHandler.consumeChar
    val currentPosition = fileHandler.currentPosition.copy()

    fileHandler.consumeChar match {
      case c
          if TokenType
            .fromLexem(c.toString() + fileHandler.currentChar)
            .isDefined =>
        val l = c.toString() + fileHandler.consumeChar
        Some(Token(TokenType.fromLexem(l).get, l, currentPosition))

      case '/' =>
        if (fileHandler.currentChar == '/') {
          while (fileHandler.currentChar != '\n' && fileHandler.currentChar != EOF) fileHandler.consumeChar
          return getNextToken
        }
        Some(Token(TokenType.Slash, '/', currentPosition))

      case c if TokenType.fromLexem(c).isDefined =>
        Some(Token(TokenType.fromLexem(c).get, c, currentPosition))

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

  def messageWithPositionInFile(lex: String = "",
                                currentPosition: CurrentPosition) =
    s"${currentPosition}  ${lex}"

  def getStringToken(position: CurrentPosition): Option[Token] = {
    var str: String = ""
    var previousChar = fileHandler.currentChar
    while (fileHandler.currentChar != '"' && fileHandler.currentChar != EOF) {
      previousChar = fileHandler.consumeChar
      if (previousChar == '\\' && fileHandler.currentChar == '"') {
        str += previousChar
        previousChar = fileHandler.consumeChar
      }
      str += previousChar
    }
    fileHandler.consumeChar
    Some(Token(TokenType.String, str, position))
  }

  def getIdentifierToken(c: Char, position: CurrentPosition): Option[Token] = {
    var lexem = c.toString()
    while (fileHandler.currentChar.isLetterOrDigit || fileHandler.currentChar == '_') lexem += fileHandler.consumeChar
    if (TokenType.fromLexem(lexem).isDefined)
      return Some(Token(TokenType.fromLexem(lexem).get, lexem, position))
    Some(Token(TokenType.Identifier, lexem, position))
  }

  def getNumberToken(c: Char, position: CurrentPosition): Option[Token] = {
    var retToken: Option[Token] = None
    var num: String = ""
    if (c == '0')
      retToken = Some(Token(TokenType.Number, c, position))
    else {
      num = c.toString
      while (fileHandler.currentChar.isDigit) num += fileHandler.consumeChar
      retToken = Some(Token(TokenType.Number, num, position))
    }
    if (fileHandler.currentChar.isLetter) {
      ExceptionHandler.reportException(
        LexerException(LexerExceptionType.IdentifierStartedWithDigit),
        Some(messageWithPositionInFile(num, position)))
    }
    retToken
  }
  implicit def charToString(c: Char): String = c.toString()
}
