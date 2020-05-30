package slang.interpreter

import slang.instructions.{Expr, Statements}
import slang.instructions.Statements._
import slang.lexer.TokenType.TokenType
import slang.parser.Parser
import slang.utils.{
  ExceptionHandler,
  MyRuntimeException,
  MyRuntimeExceptionType
}

import scala.collection.mutable.ListBuffer

case class Interpreter(parser: Parser) {
  var currentScope = Scope()

}
