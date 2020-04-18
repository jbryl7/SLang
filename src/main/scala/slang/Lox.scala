package slang

import slang.lexer.{Lexer, Reader, Token, TokenType}

import scala.io.Source

object Lox extends App {
  val EX_USAGE = 64
  args match {
    case arr if arr.length > 1 =>
      println(f"Usage: run [input file]")
      scala.sys.exit(EX_USAGE)
    case arr if arr.length == 1 =>
      val reader = Reader(Some(arr.head))
      val lexer = Lexer(reader)
      var token: Option[Token] = lexer.getNextToken
      println(token)
      while (token.isDefined && token.get.tokenType != TokenType.EOF) {
        token = lexer.getNextToken
        println(token)
      }

  }
}
