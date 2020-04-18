package slang.lexer
import slang._

case class Lexer(reader: Reader) {
  def getNextToken: Option[Token] = {

    while (reader.currentChar.isWhitespace || reader.currentChar == '\n') reader.consumeChar
    while (!reader.currentChar.isWhitespace) {
      if (reader.currentChar == EOF)
        return Some(Token(TokenType.EOF, EOF, reader.row))

      reader.consumeChar match {
        case c if List('=', '!', '<', '>').contains(c) =>
          if (reader.currentChar == '=') {
            val l = c.toString + "="
            reader.consumeChar
            return Some(Token(TokenType.fromLexem(l).get, l, reader.row))
          }
          return Some(Token(TokenType.fromLexem(c).get, c, reader.row))

        case '/' =>
          if (reader.currentChar == '/') {
            while (reader.currentChar != '\n' && reader.currentChar != EOF) reader.consumeChar
            return getNextToken
          }
          return Some(Token(TokenType.Slash, '/', reader.row))

        case c if TokenType.fromLexem(c).isDefined =>
          return Some(Token(TokenType.fromLexem(c).get, c, reader.row))

        case '"' => // string
          var str: String = ""
          var previousChar = reader.currentChar
          while (reader.currentChar != '"' && reader.currentChar != EOF) {
            previousChar = reader.consumeChar
            if (previousChar == '\\' && reader.currentChar == '"') {
              str += previousChar
              previousChar = reader.consumeChar
            }
            str += previousChar
          }
          reader.consumeChar
          return Some(Token(TokenType.String, str, reader.row))

        case c if c.isDigit => //idea: maybe add float Type
          if (c == '0')
            return Some(Token(TokenType.Number, c, reader.row))

          var num: String = c.toString
          while (reader.currentChar.isDigit) num += reader.consumeChar
          num.foreach { d =>
            if (!d.isDigit) {
              ExceptionHandler.reportException(
                AnalysePhase.Lexer,
                LexerException(LexerExceptionType.IdentifierStartedWithDigit),
                Some(messageWithPosition(num)))
              return None
            }
          }
          return Some(Token(TokenType.Number, num, reader.row))

        case c if c.isLetter || c == '_' => // identificator or keyword
          var lexem = c.toString()
          while (reader.currentChar.isLetterOrDigit || reader.currentChar == '_') lexem += reader.consumeChar
          if (TokenType.fromLexem(lexem).isDefined)
            return Some(
              Token(TokenType.fromLexem(lexem).get, lexem, reader.row))
          return Some(Token(TokenType.Identifier, lexem, reader.row))

        case c =>
          ExceptionHandler.reportException(
            AnalysePhase.Lexer,
            LexerException(LexerExceptionType.InvalidSyntax),
            Some(messageWithPosition()))
          return None
      }
    }
    None
  }
  def messageWithPosition(lex: String = "") =
    s"line ${reader.row} column ${reader.column}  ${lex}"
  implicit def charToString(c: Char): String =
    c.toString()
}
