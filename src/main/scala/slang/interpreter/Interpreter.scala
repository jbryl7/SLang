package slang.interpreter

import slang.instructions.Statements
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

  def interpret(): Int = {
    val rootBlock = parser.parse()
    println(rootBlock)
    println("interpreting")
    rootBlock.statements.foreach(visit(_))
    0
  }
  def visit(Statements: Statements): Int = {
    Statements match {
      case instruction: FunctionStatement => visit(instruction)
      case instruction: ClassBody           => visit(instruction)
      case instruction: Block               => visit(instruction)
      case instruction: VarStatement      => visit(instruction)
      case instruction: ClassStatement => visit(instruction)
      case instruction: ExpressionStatement      => visit(instruction)
      case instruction: PrintStatement     => visit(instruction)
      case instruction: ReturnStatement              => visit(instruction)
    }
    0
  }
  def visit(returnStatement: ReturnStatement): Int = {
    println("return")
    visit(returnStatement.value)
  }

  def visit(functionDeclaration: FunctionStatement): Int = {
    println("funDeclaration")
    currentScope.addFunction(functionDeclaration)
    0
  }

  def visit(varDeclaration: VarStatement): Int = {
    println("var declaration")
    currentScope.addVariable(varDeclaration)
    0
  }
  def visit(classDeclaration: ClassStatement): Int = {
    println("class declaration")
    currentScope.addClass(classDeclaration)
    0
  }

  def checkReturnType(retVal: Int, returnType: TokenType): Boolean =
    true //todo

  def execFunction(functionDeclaration: FunctionStatement,
                   arguments: ListBuffer[Statements]): Int = {

    println("executing function")
    addScope()
    if (functionDeclaration.params.length != arguments.length)
      ExceptionHandler.reportException(
        MyRuntimeException(MyRuntimeExceptionType.InvalidNumberOfArguments),
        Some(
          f"${functionDeclaration.name} expects ${functionDeclaration.params.length} args")
      )

    0
  }
  def visit(ifStatement: IfStatement): Int = {
    println("ifelse")
    if (visit(ifStatement.condition))
      visit(ifStatement.thenBlock)
    else if (ifStatement.elseBlock != null)
      visit(ifStatement.elseBlock)
    else
      0
  }
  def visit(expression: ExpressionStatement): Int = {
    println("expression")
    0
  }

  def visit(block: Block): Int = {
    println("block")
    addScope()
    block.statements.foreach(visit(_))
    destroyCurrentScope()
    0
  }

  def addScope() = {
    println("adding scope")
    val newScope = Scope()
    newScope.parentScope = Some(currentScope)
    currentScope = newScope
  }
  def destroyCurrentScope() = {
    println("destroying scope")
    currentScope = currentScope.parentScope.getOrElse(Scope())
  }
//}
