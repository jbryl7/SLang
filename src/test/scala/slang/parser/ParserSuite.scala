package slang.parser

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import slang.{EOF, lexer}
import org.scalatest.matchers._
import slang.instructions.Program
import slang.lexer.{FileHandler, Lexer, Token}
import slang.utils.{LexerException, ParserException}

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

class ParserSuite extends AnyFlatSpec with Matchers {

  def parseCode(code: String): Program = {
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
  "unexpected token" should "throw exception" in {
    an[ParserException] should be thrownBy parseCode("x > 5")
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
  "correct var declaration from expression" should "not throw exception" in {
    noException should be thrownBy parseCode(
      "var x: Int = (0 + 1) * (15 - 10) + 3")
  }
  "correct fun declaration" should "not throw exception" in {
    noException should be thrownBy parseCode("def foo(x: Int): Int = {}")
  }
  "correct fun one line declaration" should "not throw exception" in {
    noException should be thrownBy parseCode("def foo(x: Int): Int =\nfoo()")
  }
  "correct if" should "not throw exception" in {
    noException should be thrownBy parseCode("if (x) {}")
  }
  "correct if else" should "not throw exception" in {
    noException should be thrownBy parseCode("if (x) {} else f()")
  }
  "correct if one liner" should "not throw exception" in {
    noException should be thrownBy parseCode("if (x)\nfun()")
  }

}
