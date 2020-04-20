package slang

import slang.lexer.{Lexer, FileHandler, Token, TokenType}

import scala.io.Source

object SLang extends App {
  try {
    args match {
      case arr if arr.length != 1 =>
        println(f"Usage: run [input file]")
        ()
      case arr =>
        val fileHandler = FileHandler(Some(arr.head))
        val lexer = Lexer(fileHandler)
        var token: Option[Token] = lexer.getNextToken
        println(token)
        while (token.isDefined && token.get.tokenType != TokenType.Eof) {
          token = lexer.getNextToken
          println(token)
        }
    }
  } catch {
    case e: Exception => println(e)
  }
}
