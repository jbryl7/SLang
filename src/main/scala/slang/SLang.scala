package slang

import slang.lexer.{FileHandler, Lexer, Token, TokenType}
import slang.parser.Parser

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
        val parser = Parser(lexer)
        val program = parser.parse()
        fileHandler.source.close()
    }
  } catch {
    case e: Exception => println(e)
  }
}
