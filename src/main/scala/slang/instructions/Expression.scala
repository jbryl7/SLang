package slang.instructions

import slang.lexer.TokenType.TokenType

import scala.collection.mutable.ListBuffer

class Expression(operands: ListBuffer[Node] = ListBuffer(),
                 operators: ListBuffer[TokenType] = ListBuffer())
    extends Node {

  def addOperator(operator: TokenType): Unit = operators.append(operator)
  def addOperand(operand: Node): Unit = operands.append(operand)

  def execute(scope: Scope) = ???
  override def toString(nested: Int) = {
    val nest = getNest(nested)
    f"\n${nest}Expression\n${nest} operators:${operators.map(_.toString).mkString}\n${nest} operands: ${operands
      .map(_.toString(nested + 1))
      .mkString}"
  }
}
