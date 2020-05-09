package slang.lexer

trait LexerInterface {
  def getNextToken: Option[Token]
  val fileHandler: FileHandler
}
