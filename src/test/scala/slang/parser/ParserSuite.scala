package slang.parser

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import slang.{EOF, lexer}
import org.scalatest.matchers._
import slang.instructions.statements.Block
import slang.lexer.{FileHandler, Lexer, Token}
import slang.utils.{LexerException, ParserException}

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

class ParserSuite extends AnyFlatSpec with Matchers {

  def parseCode(code: String): Block = {
    Parser(Lexer(FileHandler(code = Some(code)))).parse()
  }

  "unexpectedEndOfFile1" should "throw exception" in {
    an[ParserException] should be thrownBy parseCode("fun(arg")
  }
  "unexpectedEndOfFile2" should "throw exception" in {
    an[ParserException] should be thrownBy parseCode("def")
  }
  "unexpectedEndOfFile3" should "throw exception" in {
    an[ParserException] should be thrownBy parseCode(
      "if (correctCondition) {} else")
  }
  "unexpectedEndOfFile4" should "throw exception" in {
    an[ParserException] should be thrownBy parseCode("def fun(arg: Int) = ")
  }
  "if else else" should "throw exception" in {
    an[ParserException] should be thrownBy parseCode(
      "if (x) {} else f() else f()")
  }
  "correct funCall" should "not throw exception" in {
    noException should be thrownBy parseCode("if (x) {} else f()")
  }
  "correct var declaration" should "not throw exception" in {
    noException should be thrownBy parseCode("var x: Int = 0")
  }
  "correct var declaration from complex expression" should "not throw exception" in {
    noException should be thrownBy parseCode(
      "var x: Int = (0 + 1) * (15 - 10) + 3")
  }
  "correct class declaration" should "not throw exception" in {
    noException should be thrownBy parseCode(
      "class XD(x: Int, h: String) {\n    def x(arg1: Int): Unit = print(\"klasa XD metoda x\")\n}")
  }
  "correct class declaration2" should "not throw exception" in {
    noException should be thrownBy parseCode("class XD(x: Int, h: String) {}")
  }
  "correct fun declaration" should "not throw exception" in {
    noException should be thrownBy parseCode("def foo(x: Int): Int = {}")
  }
  "correct fun one line declaration" should "not throw exception" in {
    noException should be thrownBy parseCode("def foo(x: Int): Int =\nfoo()")
  }
  "if (x) {}" should "not throw exception" in {
    noException should be thrownBy parseCode("if (x) {}")
  }
  "obj.another.fun()" should "not throw exception" in {
    noException should be thrownBy parseCode("obj.another.fun()")
  }
  "if (x) {} else f()" should "not throw exception" in {
    noException should be thrownBy parseCode("if (x) {} else f()")
  }
  "if (x)\\nfun()" should "not throw exception" in {
    noException should be thrownBy parseCode("if (x)\nfun()")
  }
  "var x: Int = 24\\n x = 42" should "not throw exception" in {
    noException should be thrownBy parseCode("var x: Int = 24\n x = 42")
  }
  "x.y().z" should "not throw exception" in {
    noException should be thrownBy parseCode("x.y().z")
  }

}
