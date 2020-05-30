package slang

//import slang.interpreter.Interpreter
import slang.instructions.Expr._
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
        val rootNode = parser.parse()
        if (rootNode == null)
          print("ERRRRRRRRRRRRRRRRRRROR")
        fileHandler.source.close()
    }
  } catch {
    case e: Exception => println(e)
  }
}
