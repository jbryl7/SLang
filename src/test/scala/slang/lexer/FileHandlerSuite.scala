package slang.lexer

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import slang.EOF
import slang.utils.FileHandlerException

class FileHandlerSuite extends AnyFlatSpec with Matchers {
  "provided with string" should "return correct list of chars" in {
    val s = "string"
    getStringReadByFileHandler(s) shouldEqual s + EOF
  }
  "provided with string with white chars" should "return correct list of chars" in {
    val s = "string\n\n\t"
    getStringReadByFileHandler(s) shouldEqual s + EOF
  }
  "provided with emtpy input" should "return EOF" in {
    val s = ""
    getStringReadByFileHandler(s) shouldEqual s + EOF
  }
  "not providing resources" should "throw exception" in {
    an[FileHandlerException] should be thrownBy FileHandler(None, None)
  }
  "providing wrong path" should "throw exception" in {
    an[FileHandlerException] should be thrownBy FileHandler(
      Some("wrong-path-424242"),
      None)
  }

  def getStringReadByFileHandler(s: String): String = {
    val fileHandler = FileHandler(code = Some(s))
    var retString = ""
    while (fileHandler.readChar != EOF) retString = retString + fileHandler.consumeChar
    retString = retString + fileHandler.consumeChar
    retString
  }
}
