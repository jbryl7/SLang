package slang.instructions
import slang.instructions.Statements.Visitor
import slang.lexer.Token

import scala.collection.mutable.ListBuffer

object Statements {
  trait Visitor[R] {
    def visitBlockStmt(stmt: Statements.Block): R
    def visitClassStmt(stmt: Statements.ClassStatement): R
    def visitExpressionStmt(stmt: Statements.ExpressionStatement): R
    def visitFunctionStmt(stmt: Statements.FunctionStatement): R
    def visitIfStmt(stmt: Statements.IfStatement): R
    def visitPrintStmt(stmt: Statements.PrintStatement): R
    def visitReturnStmt(stmt: Statements.ReturnStatement): R
    def visitVarStmt(stmt: Statements.VarStatement): R
  }

  case class Block(statements: ListBuffer[Statements]) extends Statements {
    override def accept[R](visitor: Visitor[R]): R =
      visitor.visitBlockStmt(this)
    override def toString: String = toString(0)
    override def toString(nested: Int): String = {
      val nest = getNest(nested)
      f"\n${nest}Block\n ${nest}statements:${statements.map(_.toString(nested + 2)).mkString}"
    }
  }

  case class ClassStatement(name: Token, classBody: ClassBody)
      extends Statements {

    override def accept[R](visitor: Visitor[R]): R =
      visitor.visitClassStmt(this)
    override def toString(nested: Int): String = {
      val nest = getNest(nested)
      f"\n${nest}Class\n ${nest}name${name}\n ${nest}classBody${classBody
        .toString(nested + 1)}"
    }
  }

  case class ExpressionStatement(expression: Expr) extends Statements {
    override def accept[R](visitor: Visitor[R]): R =
      visitor.visitExpressionStmt(this)
    override def toString(nested: Int): String = {
      val nest = getNest(nested)
      f"\n${nest}Expression${expression.toString(nested + 1)}"
    }
  }

  case class FunctionStatement(name: Token,
                               returnType: Token,
                               params: ListBuffer[Parameter],
                               body: Block)
      extends Statements {

    override def accept[R](visitor: Visitor[R]): R =
      visitor.visitFunctionStmt(this)

    override def toString(nested: Int): String = {
      val nest = getNest(nested)
      f"\n${nest}Function\n ${nest}name${name}\n${nest} params:${params
        .map(_.toString(nested + 2))
        .mkString}${body.toString(nested + 1)}"
    }
  }

  case class IfStatement(condition: Expr,
                         thenBlock: Statements,
                         elseBlock: Statements)
      extends Statements {

    override def accept[R](visitor: Visitor[R]): R =
      visitor.visitIfStmt(this)

    override def toString(nested: Int): String = {
      val nest = getNest(nested)
      f"\n${nest}IfStatement\n${nest} condition${condition.toString(nested + 2)}\n${nest} thenBlock${thenBlock
        .toString(nested + 2)}\n${nest} elseBlock${elseBlock.toString(nested + 2)}"
    }
  }

  case class PrintStatement(expression: Expr) extends Statements {

    override def accept[R](visitor: Visitor[R]): R =
      visitor.visitPrintStmt(this)

    override def toString(nested: Int): String = {
      val nest = getNest(nested)
      f"\n${nest}Print${expression.toString(nested + 1)}"
    }
  }

  case class ReturnStatement(keyword: Token, value: Expr) extends Statements {

    override def accept[R](visitor: Visitor[R]): R =
      visitor.visitReturnStmt(this)

    override def toString(nested: Int): String = {
      val nest = getNest(nested)
      f"\n${nest}Return\n ${nest}keyword${keyword.toString(nested + 1)}\n${nest} value${value
        .toString(nested + 1)}"
    }
  }

  case class VarStatement(name: Token, initializer: Expr, varType: Token)
      extends Statements {

    override def accept[R](visitor: Visitor[R]): R =
      visitor.visitVarStmt(this)

    override def toString(nested: Int): String = {
      val nest = getNest(nested)
      f"\n${nest}Var\n ${nest}name:${name.toString(nested + 2)} \n${nest} initializer:${initializer
        .toString(nested + 2)}\n${nest} varType: ${varType.toString(nested + 2)}"
    }
  }
  case class ClassBody(vars: ListBuffer[VarStatement] = ListBuffer(),
                       funs: ListBuffer[FunctionStatement] = ListBuffer())
      extends Node {

    def toString(nested: Int): String = {
      val nest = getNest(nested)
      f"\n${nest} classBody \n${nest} vars:${vars
        .map(_.toString(nested + 1))
        .mkString("")}\n ${nest}functions: ${funs.map(_.toString(nested + 1)).mkString("")}"
    }
  }
}

abstract class Statements extends Node {

  def accept[R](visitor: Statements.Visitor[R]): R
  def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest} Statement"
  }

}
