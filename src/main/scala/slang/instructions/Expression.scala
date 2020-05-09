package slang.instructions

import slang.lexer.TokenType.TokenType

import scala.collection.mutable.ListBuffer

class Expression(operators: ListBuffer[TokenType] = ListBuffer(),
                 operands: ListBuffer[Node] = ListBuffer())
    extends Node
    with Instruction {

  def addOperator(operator: TokenType): Unit = operators.append(operator)
  def addOperand(operand: Node): Unit = operands.append(operand)

  def execute(scope: Scope) = ???
  override def toString() =
    f"Expression(operators: ${operators} operands: ${operands}"
}
