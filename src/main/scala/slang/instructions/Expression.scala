package slang.instructions

import slang.lexer.TokenType.TokenType

import scala.collection.mutable.ListBuffer

class Expression extends Node with Instruction {
  val operators: ListBuffer[TokenType] = ListBuffer()
  val operands: ListBuffer[Node] = ListBuffer()

  def addOperator(operator: TokenType) = operators.append(operator)
  def addOperand(operand: Node) = operands.append(operand)

  def getOperators() = operators
  def getOperands() = operands

  def execute(scope: Scope) = ???
  def evaluate() = ???
}
