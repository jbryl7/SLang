package slang.lexer

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import slang.EOF
import org.scalatest.matchers._
import slang.utils.LexerException

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

class LexerSuite extends AnyFlatSpec with Matchers {
  val escapedString = "\"string with \\\"escaping\\\"\""
  val escapedStringAfterLexer = "string with \\\"escaping\\\""
  val escapedStringToken: Token =
    Token(TokenType.String, escapedStringAfterLexer, 0)
  def eofToken(line: Int = 0): Token = Token(TokenType.EOF, EOF.toString, line)
  val escapedStringWithComment: String = escapedString + "// comment"

  val fun: String = "def foo(): Int = "
  val funTokens: List[Token] = List(
    Token(TokenType.Fun, "def", 0),
    Token(TokenType.Identifier, "foo", 0),
    Token(TokenType.LeftParenthesis, "(", 0),
    Token(TokenType.RightParenthesis, ")", 0),
    Token(TokenType.Colon, ":", 0),
    Token(TokenType.Type, "Int", 0),
    Token(TokenType.Assign, "=", 0),
    eofToken()
  )
  "number in source" should "return correct number token" in {
    val number = "42"
    val returnedTokens = getTokensForCode(number)
    returnedTokens shouldEqual List(Token(TokenType.Number, number, 0),
                                    eofToken()).map(Some(_))
  }
  "escaped string" should "return correct string token" in {
    val returnedTokens = getTokensForCode(escapedString)
    returnedTokens shouldEqual List(escapedStringToken, eofToken()).map(Some(_))

  }
  "escaped string with comment" should "return correct string token" in {
    val returnedTokens = getTokensForCode(escapedStringWithComment)
    returnedTokens shouldEqual List(escapedStringToken, eofToken()).map(Some(_))
  }
  "correct fun definition" should "return correct list of tokens" in {
    val returnedTokens = getTokensForCode(fun)
    returnedTokens shouldEqual funTokens.map(Some(_))
  }
  "# as first char of identifier" should "throw exception" in {
    an[LexerException] should be thrownBy getTokensForCode("#identifier")
  }
  "identifier starting with a digit" should "throw exception" in {
    an[LexerException] should be thrownBy getTokensForCode("42identifier")
  }

  def getTokensForCode(code: String): List[Option[Token]] = {
    val lexer = Lexer(FileHandler(code = Some(code)))
    var token: Option[Token] = lexer.getNextToken
    val tokens: ListBuffer[Option[Token]] = ListBuffer(token)
    while (token.isDefined && token.get.tokenType != TokenType.EOF) {
      token = lexer.getNextToken
      tokens += token
    }
    tokens.toList
  }

}
