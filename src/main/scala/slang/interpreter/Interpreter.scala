package slang.interpreter

import slang.instructions.expressions._
import slang.instructions.statements.{
  Block,
  ClassStatement,
  ExpressionStatement,
  FunctionStatement,
  IfStatement,
  PrintStatement,
  ReturnStatement,
  StatementVisitor,
  VarStatement
}
import slang.interpreter.runtimeclasses.{MyCallable, MyFunction, MyInstance}
import slang.lexer.{Token, TokenType}
import slang.parser.Parser
import slang.utils.{
  ExceptionHandler,
  MyRuntimeException,
  MyRuntimeExceptionType
}
case class Interpreter(parser: Parser)
    extends StatementVisitor[Unit]
    with ExpressionVisitor[Any] {

  var currentScope: Scope = Scope()

  def interpret(): Unit = {
    val rootBlock = parser.parse()
    rootBlock.statements.foreach(execute)
  }

  def interpret(expression: Expression): Unit =
    try {
      println(evaluate(expression))
    } catch {
      case e: Throwable => println(e)
    }

  override def visitBlockStmt(stmt: Block): Unit = {
    currentScope = Scope(parentScope = currentScope)
    stmt.statements.foreach(_.accept(this))
    currentScope = currentScope.parentScope
  }

  override def visitClassStmt(stmt: ClassStatement): Unit = ???

  override def visitExpressionStmt(stmt: ExpressionStatement): Unit = {
    println("visit expression stmt")
    evaluate(stmt.expression)
  }

  override def visitFunctionStmt(stmt: FunctionStatement): Unit = {
    if (!currentScope.isInScope(stmt.name.lexeme))
      currentScope.define(stmt.name, MyFunction(stmt))
  }

  override def visitIfStmt(stmt: IfStatement): Unit = {
    if (evaluate(stmt.condition).asInstanceOf[Boolean])
      execute(stmt.thenBlock)
    else if (stmt.elseBlock != null)
      execute(stmt.elseBlock)
  }

  override def visitPrintStmt(stmt: PrintStatement): Unit =
    println(evaluate(stmt.expression).toString)

  override def visitReturnStmt(stmt: ReturnStatement): Unit = {
    var value: Any = null
    if (stmt.value != null) {
      value = evaluate(stmt.value)
    }
    throw Return(value)
  }

  override def visitVarStmt(stmt: VarStatement): Unit = {
    var value: Any = null
    if (stmt.initializer != null)
      value = evaluate(stmt.initializer)
    //add typeChecks
    currentScope.define(stmt.name, value)
  }

  override def visitSetExpr(set: SetExpression) = ???

  override def visitAssignExpr(expr: AssignExpression): Any = {
    currentScope.set(expr.name, expr.value)
  }

  override def visitBinaryExpr(expr: BinaryExpression): Any = {
    def reportOperandMustBeANumber(operator: Token): Any =
      ExceptionHandler.reportException(
        MyRuntimeException(MyRuntimeExceptionType.OperandMustBeANumber),
        Some(operator.toString))
    def checkEquality(left: Any, right: Any): Boolean =
      (left, right) match {
        case (null, null)          => true
        case (null, _) | (_, null) => false
        case (l, r)                => l.equals(r)
      }

    val left = evaluate(expr.left)
    val right = evaluate(expr.right)

    expr.operator.tokenType match {
      case TokenType.Minus => left.asInstanceOf[Int] - right.asInstanceOf[Int]
      case TokenType.Plus =>
        (left, right) match {
          case (l: Int, r: Int) => l + r
          case (l: String, r) =>
            try { l + r.toString } catch {
              case _: Throwable =>
                ExceptionHandler.reportException(
                  MyRuntimeException(
                    MyRuntimeExceptionType.SumIncompatibleTypes),
                  Some(expr.right.toString))
            }
          case _ =>
            ExceptionHandler.reportException(
              MyRuntimeException(MyRuntimeExceptionType.SumIncompatibleTypes),
              Some(expr.right.toString))
        }

      case TokenType.MultiplicativeOperator =>
        (left, right) match {
          case (l: Int, r: Int) => l * r
          case _                => reportOperandMustBeANumber(expr.operator)
        }
      case TokenType.DivideOperator =>
        (left, right) match {
          case (l: Int, r: Int) => l / r
          case _                => reportOperandMustBeANumber(expr.operator)
        }
      case TokenType.LessEqual =>
        (left, right) match {
          case (l: Int, r: Int) => l <= r
          case _                => reportOperandMustBeANumber(expr.operator)
        }
      case TokenType.Equal =>
        checkEquality(left, right)
      case TokenType.BangEqual =>
        !checkEquality(left, right)

      case TokenType.Less =>
        (left, right) match {
          case (l: Int, r: Int) => l < r
          case _                => reportOperandMustBeANumber(expr.operator)
        }
      case TokenType.GreaterEqual =>
        (left, right) match {
          case (l: Int, r: Int) => l >= r
          case _                => reportOperandMustBeANumber(expr.operator)
        }
      case TokenType.Greater =>
        (left, right) match {
          case (l: Int, r: Int) => l > r
          case _                => reportOperandMustBeANumber(expr.operator)
        }
      case _ =>
        println("interpreting binary expression error")
        null
    }

  }

  override def visitCallExpr(expr: CallExpression): Any = {
    val calle = evaluate(expr.callee)
    val args = expr.arguments.map(evaluate).toList
    if (!calle.isInstanceOf[MyCallable])
      ExceptionHandler.reportException(
        MyRuntimeException(
          MyRuntimeExceptionType.YouCanCallOnlyFunctionsAndClasses),
        Some(expr.paren.position.toString))
    calle.asInstanceOf[MyCallable].call(this, args)
  }

  override def visitGetExpr(expr: GetExpression): Any = {
    val obj = evaluate(expr.`object`)
    obj match {
      case myInstance: MyInstance => myInstance.get(expr.name)
    }
  }

  override def visitGroupingExpr(expr: GroupingExpression): Any =
    evaluate(expr.expression)

  override def visitLiteralExpr(expr: LiteralExpression): Any =
    expr.value

  override def visitLogicalExpr(expr: LogicalExpression): Any = {
    val l = evaluate(expr.left)
    expr.operator.tokenType match {
      case TokenType.And =>
        isTrue(l) && isTrue(evaluate(expr.right))
      case TokenType.Or =>
        isTrue(l) || isTrue(evaluate(expr.right))
      case _ =>
        println("interpreting logicalExpressionError")
        null
    }
  }

  override def visitThisExpr(expr: ThisExpression): Any = ???
  def isTrue(r: Any): Boolean =
    r match {
      case r: Boolean => r
      case ""         => false
      case 0          => false
      case null       => false
      case _          => true
    }
  override def visitUnaryExpr(expr: UnaryExpression): Any = {
    val right = evaluate(expr.right)
    try {
      expr.operator.tokenType match {
        case TokenType.Minus =>
          -1 * right.asInstanceOf[Int]
        case TokenType.Bang =>
          !isTrue(right)
        case _ =>
          null
      }
    } catch {
      case _: Throwable =>
        ExceptionHandler.reportException(
          MyRuntimeException(MyRuntimeExceptionType.MinusNotANumber))
        null
    }
  }

  override def visitVariableExpr(expr: VariableExpression): Any =
    currentScope.get(expr.name)
}
