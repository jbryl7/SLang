package slang.instructions

import slang.lexer.TokenType.TokenType

import scala.collection.mutable.ListBuffer

class Expression(operators: ListBuffer[TokenType] = ListBuffer(),
                 operands: ListBuffer[Node] = ListBuffer())
    extends Node
    with Instruction {

  def addOperator(operator: TokenType) = operators.append(operator)
  def addOperand(operand: Node) = operands.append(operand)

  def getOperators() = operators
  def getOperands() = operands

  def execute(scope: Scope) = ???
  def evaluate() = ???
  override def toString() =
    f"Expression(\noperators: ${operators},\noperands:${operands})"
}
