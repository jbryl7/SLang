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
  "declaration of fun when variable with identical identifier in scope" should "throw exception" in {
    an[ParserException] should be thrownBy parseCode(
      "var x: Int = 24\ndef x() = {}")
  }
  "redeclaration of variable" should "throw exception" in {
    an[ParserException] should be thrownBy parseCode(
      "var x: Int = 24\n var x: Int = 42")
  }
  "if else else" should "throw exception" in {
    an[ParserException] should be thrownBy parseCode(
      "if (x) {} else f() else f()")
  }
  "declaring variable with identifier equal to one of parameters in function body" should "throw exception" in {
    an[ParserException] should be thrownBy parseCode(
      "\ndef foo3(x: Int, y: Int, z: Int): Int =\n    var y: Int = 4")
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
    noException should be thrownBy parseCode("class Class { var x: Int = 5 }")
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
  "declare in different blocks" should "not throw exception" in {
    noException should be thrownBy parseCode("{var x: Int = 5} var x: Int = 5")
  }
  "declaring variables in different scopes" should "not throw exception" in {
    noException should be thrownBy parseCode(
      "def fooq(x: Int, y: Int, z: Int): Int =\n    foo2()\ndef foo3(x: Int, y: Int, z: Int): Int =\n    var b: Int = 4")
  }
  "declaring variable with the same identifier in class and in scope" should "not throw exception" in {
    noException should be thrownBy parseCode(
      "class Class { var x: Int = 5 }\nvar x: Class = Class()")
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

}
