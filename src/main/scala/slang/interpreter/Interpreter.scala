package slang.interpreter

import slang.instructions.expressions._
import slang.parser.Parser
case class Interpreter(parser: Parser) extends StatementVisitorImpl {

  var currentScope: Scope = Scope()

  def interpret(): Unit = {
    val rootBlock = parser.parse()
    print(rootBlock)
    rootBlock.statements.foreach(execute)
  }

  def interpret(expression: Expression): Unit =
    try {
      println(evaluate(expression))
    } catch {
      case e: Throwable => println(e)
    }

}
