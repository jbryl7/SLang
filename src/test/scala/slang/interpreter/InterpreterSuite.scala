package slang.interpreter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import slang.instructions.statements.Block
import slang.lexer.{FileHandler, Lexer}
import slang.parser.Parser
import slang.utils.{MyRuntimeException, ParserException}

import scala.language.postfixOps

class InterpreterSuite extends AnyFlatSpec with Matchers {
  def intepretCode(code: String): Unit =
    Interpreter(Parser(Lexer(FileHandler(code = Some(code))))).interpret()

  "undefined fun" should "throw exception" in {
    an[MyRuntimeException] should be thrownBy intepretCode(
      "if (false) {} else f()")
  }
  "ZeroDivision" should "throw exception" in {
    an[MyRuntimeException] should be thrownBy intepretCode("var x: Int = 5 / 0")
  }

  "accessing non-existing object param" should "throw exception" in {
    an[MyRuntimeException] should be thrownBy intepretCode(
      "class K1(x: Int) {\n    def foo(x: Int): Int =\n        return this.x + x\n}\nclass K2(k: K1) {}\nvar k1: K1 = K1(10)\nvar k2: Int = k1.y")
  }
  "accessing non-existing object method" should "throw exception" in {
    an[MyRuntimeException] should be thrownBy intepretCode(
      "class K1(x: Int) {\n    def foo(x: Int): Int =\n        return this.x + x\n}\nclass K2(k: K1) {}\nvar k1: K1 = K1(10)\nvar k2: Int = k1.f()")
  }
  "returns value from object method" should "throw exception" in {
    noException should be thrownBy intepretCode(
      "class K1(x: Int) {\n    def foo(x: Int): Int =\n        return this.x + x\n}\n\nvar k1: K1 = K1(10)\nvar k2: Int = k1.x")
  }
  "correct funCall" should "not throw exception" in {
    noException should be thrownBy intepretCode(
      "def f(): Unit = print(5) \n if (false) {} else f()")
  }
  "correct var declaration" should "not throw exception" in {
    noException should be thrownBy intepretCode("var x: Int = 0")
  }
  "correct var declaration from complex expression" should "not throw exception" in {
    noException should be thrownBy intepretCode(
      "var x: Int = (0 + 1) * (15 - 10) + 3")
  }
  "correct class declaration" should "not throw exception" in {
    noException should be thrownBy intepretCode(
      "class XD(x: Int, h: String) {\n    def x(arg1: Int): Unit = print(\"klasa XD metoda x\")\n}")
  }
  "correct class declaration and then create object" should "not throw exception" in {
    noException should be thrownBy intepretCode(
      "class XD(x: Int, h: String) {}\n var x: XD = XD(5, \"napis\")")
  }
  "correct fun declaration and then call" should "not throw exception" in {
    noException should be thrownBy intepretCode(
      "def foo(x: Int): Int = {return 5} foo(5)")
  }
  "if (x) {}" should "not throw exception" in {
    noException should be thrownBy intepretCode(
      "var x: Boolean = true \n if (x) {}")
  }
  "var x: Int = 24\\n x = 42" should "not throw exception" in {
    noException should be thrownBy intepretCode(
      "var x: Int = 24\n x = 42 \n print(x)")
  }
  "object as parameter of another object" should "not throw exception" in {
    noException should be thrownBy intepretCode(
      "class K1(x: Int) {\n    def foo(x: Int): Int =\n        return this.x + x\n}\nclass K2(k: K1) {}\nvar k1: K1 = K1(10)\nvar k2: K2 = K2(k1)")
  }
  "set new val to object.param" should "not throw exception" in {
    noException should be thrownBy intepretCode(
      "class K1(x: Int) {\n    def foo(x: Int): Int =\n        return this.x + x\n}\nvar k1: K1 = K1(10)\nk1.x = 15")
  }

}
