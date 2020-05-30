package slang.instructions

import slang.lexer.Token

import scala.collection.mutable.ListBuffer

object Expr {
  trait Visitor[R] {
    def visitSetExpr(set: Expr.SetExpr): R
    def visitAssignExpr(expr: Expr.AssignExpr): R
    def visitBinaryExpr(expr: Expr.BinaryExpr): R
    def visitCallExpr(expr: Expr.CallExpr): R
    def visitGetExpr(expr: Expr.GetExpr): R
    def visitGroupingExpr(expr: Expr.GroupingExpr): R
    def visitLiteralExpr(expr: Expr.LiteralExpr): R
    def visitLogicalExpr(expr: Expr.LogicalExpr): R
    def visitThisExpr(expr: Expr.ThisExpr): R
    def visitUnaryExpr(expr: Expr.UnaryExpr): R
    def visitVariableExpr(expr: Expr.VariableExpr): R
  }
  case class AssignExpr(name: Token, value: Expr) extends Expr {
    override def accept[R](visitor: Expr.Visitor[R]): R =
      visitor.visitAssignExpr(this)

    override def toString(nested: Int): String = {
      val nest = getNest(nested)
      f"\n${nest} AssignExpression${name.toString(nested + 1)}${value
        .toString(nested + 1)}"
    }
  }
  case class BinaryExpr(left: Expr, operator: Token, right: Expr) extends Expr {
    override def accept[R](visitor: Expr.Visitor[R]): R =
      visitor.visitBinaryExpr(this)

    override def toString(nested: Int): String = {
      val nest = getNest(nested)
      f"\n${nest} BinaryExpression${operator.toString(nested + 1)}${left
        .toString(nested + 1)}${right.toString(nested + 1)}"
    }
  }
  case class CallExpr(callee: Expr, paren: Token, arguments: ListBuffer[Expr])
      extends Expr {
    override def accept[R](visitor: Expr.Visitor[R]): R =
      visitor.visitCallExpr(this)

    override def toString(nested: Int): String = {
      val nest = getNest(nested)
      f"\n${nest}CallExpr${callee.toString(nested + 1)}${paren
        .toString(nested + 1)}${arguments.map(_.toString(nested + 1)).mkString}"
    }
  }
  case class GetExpr(val `object`: Expr, val name: Token) extends Expr {
    override def accept[R](visitor: Expr.Visitor[R]): R =
      visitor.visitGetExpr(this)

    override def toString(nested: Int): String = {
      val nest = getNest(nested)
      f"\n${nest}GetExpression${`object`.toString(nested + 1)}${name
        .toString(nested + 1)}"
    }
  }
  case class SetExpr(val `object`: Expr, val name: Token, val value: Expr)
      extends Expr {
    override def accept[R](visitor: Expr.Visitor[R]): R =
      visitor.visitSetExpr(this)

    override def toString(nested: Int): String = {
      val nest = getNest(nested)
      f"\n${nest}SetExpression${`object`.toString(nested + 1)}${name
        .toString(nested + 1)}${value.toString(nested + 1)}"
    }
  }
  case class GroupingExpr(val expression: Expr) extends Expr {
    override def accept[R](visitor: Expr.Visitor[R]): R =
      visitor.visitGroupingExpr(this)
    override def toString(nested: Int): String = {
      val nest = getNest(nested)
      f"\n${nest}GroupingExpression${expression.toString(nested + 1)}"
    }
  }
  case class LiteralExpr(var value: AnyVal) extends Expr {
    override def accept[R](visitor: Expr.Visitor[R]): R =
      visitor.visitLiteralExpr(this)

    override def toString(nested: Int): String = {
      val nest = getNest(nested)
      f"\n${nest}LiteralExpression ${value.toString}"
    }
  }
  case class LogicalExpr(val left: Expr, val operator: Token, val right: Expr)
      extends Expr {
    override def accept[R](visitor: Expr.Visitor[R]): R =
      visitor.visitLogicalExpr(this)

    override def toString(nested: Int): String = {
      val nest = getNest(nested)
      f"\n${nest}LogicalExpression${operator.toString(nested + 1)}${left
        .toString(nested + 1)}${right.toString(nested + 1)}"
    }
  }
  case class ThisExpr(val keyword: Token) extends Expr {
    override def accept[R](visitor: Expr.Visitor[R]): R =
      visitor.visitThisExpr(this)

    override def toString(nested: Int): String = {
      val nest = getNest(nested)
      f"\n${nest}ThisExpression${keyword.toString(nested + 1)}"
    }
  }
  case class UnaryExpr(val operator: Token, val right: Expr) extends Expr {
    override def accept[R](visitor: Expr.Visitor[R]): R =
      visitor.visitUnaryExpr(this)
    override def toString(nested: Int): String = {
      val nest = getNest(nested)
      f"\n${nest}UnaryExpression${operator.toString(nested + 1)}${right
        .toString(nested + 1)}"
    }
  }
  case class VariableExpr(val name: Token) extends Expr {
    override def accept[R](visitor: Expr.Visitor[R]): R =
      visitor.visitVariableExpr(this)
    override def toString(nested: Int): String = {
      val nest = getNest(nested)
      f"\n${nest}VariableExpression${name.toString(nested + 1)}"
    }
  }
  case class MyString(value: String) extends AnyVal
  implicit def myStringToString(myString: MyString): String = myString.value
  implicit def stringToMyString(string: String): MyString = MyString(string)

}

abstract class Expr extends Node {
  def accept[R](visitor: Expr.Visitor[R]): R
  def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest} Expr"
  }
}
