package slang

import slang.interpreter.Interpreter
import slang.lexer._
import slang.parser.Parser

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
        val interpreter = Interpreter(parser)
        interpreter.interpret()
        fileHandler.source.close()
    }
  } catch {
    case e: Exception => println(e)
  }
}
