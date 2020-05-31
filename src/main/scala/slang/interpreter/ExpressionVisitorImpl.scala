package slang.interpreter

import slang.instructions.expressions._
import slang.interpreter.runtimeclasses.MyInstance
import slang.lexer.{Token, TokenType}
import slang.utils._

trait ExpressionVisitorImpl extends ExpressionVisitor[Any] {
  var currentScope: Scope

  override def visitSetExpr(set: SetExpression) = ???

  override def visitAssignExpr(expr: AssignExpression): Any = {
    currentScope.setVariable(expr.name, expr.value)
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
      case _ =>
        println("interpreting binary expression error")
        null
    }

  }

  override def visitCallExpr(expr: CallExpression): Any = {
    val calle = evaluate(expr.callee)
    val args = expr.arguments.map(evaluate)

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
    currentScope.getVariable(expr.name)
}
