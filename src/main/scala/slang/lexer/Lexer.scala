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
    while (!fileHandler.currentChar.isWhitespace) {
      if (fileHandler.currentChar == EOF)
        return Some(Token(TokenType.EOF, EOF, fileHandler.row))

      fileHandler.consumeChar match {
        case c if List('=', '!', '<', '>').contains(c) =>
          if ((c == '<' && fileHandler.currentChar == '-') || fileHandler.currentChar == '=') {
            val l = c.toString + fileHandler.consumeChar
            return Some(Token(TokenType.fromLexem(l).get, l, fileHandler.row))
          }
          return Some(Token(TokenType.fromLexem(c).get, c, fileHandler.row))

        case '/' =>
          if (fileHandler.currentChar == '/') {
            while (fileHandler.currentChar != '\n' && fileHandler.currentChar != EOF) fileHandler.consumeChar
            return getNextToken
          }
          return Some(Token(TokenType.Slash, '/', fileHandler.row))

        case c if TokenType.fromLexem(c).isDefined =>
          return Some(Token(TokenType.fromLexem(c).get, c, fileHandler.row))

        case '"' => // string
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
          return Some(Token(TokenType.String, str, fileHandler.row))

        case c if c.isDigit => //idea: maybe add float Type
          var retToken: Option[Token] = None
          var num: String = ""
          if (c == '0')
            retToken = Some(Token(TokenType.Number, c, fileHandler.row))
          else {
            num = c.toString
            while (fileHandler.currentChar.isDigit) num += fileHandler.consumeChar
            retToken = Some(Token(TokenType.Number, num, fileHandler.row))
          }

          if (fileHandler.currentChar.isLetter) {
            ExceptionHandler.reportException(
              LexerException(LexerExceptionType.IdentifierStartedWithDigit),
              Some(messageWithPositionInFile(num)))
          }

          return retToken

        case c if c.isLetter || c == '_' =>
          var lexem = c.toString()
          while (fileHandler.currentChar.isLetterOrDigit || fileHandler.currentChar == '_') lexem += fileHandler.consumeChar
          if (TokenType.fromLexem(lexem).isDefined)
            return Some(
              Token(TokenType.fromLexem(lexem).get, lexem, fileHandler.row))
          return Some(Token(TokenType.Identifier, lexem, fileHandler.row))

        case c =>
          ExceptionHandler.reportException(
            LexerException(LexerExceptionType.InvalidSyntax),
            Some(messageWithPositionInFile()))
          return None
      }
    }
    None
  }

  def messageWithPositionInFile(lex: String = "") =
    s"line ${fileHandler.row} column ${fileHandler.column}  ${lex}"

  implicit def charToString(c: Char): String =
    c.toString()
}
