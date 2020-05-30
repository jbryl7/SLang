package slang.interpreter

import slang.instructions.expressions._
import slang.instructions.statements._
import slang.lexer.{Token, TokenType}
import slang.parser.Parser
import slang.utils.{
  ExceptionHandler,
  MyRuntimeException,
  MyRuntimeExceptionType
}
case class Interpreter(parser: Parser)
    extends ExpressionVisitor[Any]
    with StatementVisitor[Unit] {

  var currentScope = Scope()
  def interpret() = {
    val rootBlock = parser.parse()
    rootBlock.statements.foreach(execute)
  }

  def interpret(expression: Expression): Unit =
    try {
      println(evaluate(expression))
    } catch {
      case e => println(e)
    }

  override def visitBlockStmt(stmt: Block) = {
    stmt.statements.foreach(_.accept(this))
  }

  override def visitClassStmt(stmt: ClassStatement) = ???

  override def visitExpressionStmt(stmt: ExpressionStatement): Unit =
    evaluate(stmt.expression)

  override def visitFunctionStmt(stmt: FunctionStatement) = ???

  override def visitIfStmt(stmt: IfStatement) = {
    if (evaluate(stmt.condition).asInstanceOf[Boolean])
      execute(stmt.thenBlock)
    else if (stmt.elseBlock != null)
      execute(stmt.elseBlock)
  }

  override def visitPrintStmt(stmt: PrintStatement) =
    println(evaluate(stmt.expression))

  override def visitReturnStmt(stmt: ReturnStatement) = ???

  override def visitVarStmt(stmt: VarStatement) = {
    var value: Any = null
    if (stmt.initializer != null)
      value = evaluate(stmt.initializer)
    //add typeChecks
    currentScope.defineVariable(stmt.name, value)
  }

  override def visitSetExpr(set: SetExpression) = ???

  override def visitAssignExpr(expr: AssignExpression) = {
    if (currentScope.isInScope(expr.name.lexeme)) {}
  }

  override def visitBinaryExpr(expr: BinaryExpression) = {
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
              case e =>
                ExceptionHandler.reportException(
                  MyRuntimeException(
                    MyRuntimeExceptionType.SumIncompatibleTypes))
            }
          case _ =>
            ExceptionHandler.reportException(
              MyRuntimeException(MyRuntimeExceptionType.SumIncompatibleTypes))
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
    }

  }

  override def visitCallExpr(expr: CallExpression) = ???

  override def visitGetExpr(expr: GetExpression) = ???

  override def visitGroupingExpr(expr: GroupingExpression) =
    evaluate(expr.expression)

  override def visitLiteralExpr(expr: LiteralExpression) =
    expr.value

  override def visitLogicalExpr(expr: LogicalExpression) = {
    val l = evaluate(expr.left)
    expr.operator.tokenType match {
      case TokenType.And =>
        isTrue(l) && isTrue(evaluate(expr.right))
      case TokenType.Or =>
        isTrue(l) || isTrue(evaluate(expr.right))
    }
  }

  override def visitThisExpr(expr: ThisExpression) = ???
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
      case _ =>
        ExceptionHandler.reportException(
          MyRuntimeException(MyRuntimeExceptionType.MinusNotANumber))
        null
    }
  }

  override def visitVariableExpr(expr: VariableExpression) =
    currentScope.getVariable(expr.name)
}
