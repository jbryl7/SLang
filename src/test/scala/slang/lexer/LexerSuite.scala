package slang.lexer

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import slang.{EOF, lexer}
import org.scalatest.matchers._
import slang.utils.LexerException

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

class LexerSuite extends AnyFlatSpec with Matchers {
  val escapedString = "\"string with \\\"escaping\\\"\""
  val escapedStringAfterLexer = "string with \\\"escaping\\\""
  val escapedStringToken: Token =
    Token(TokenType.StringLiteral,
          escapedStringAfterLexer,
          CurrentPosition(0, 0))
  val escapedStringWithComment: String = escapedString + "// comment"

  "number in source" should "return correct number token" in {
    val number = "42"
    val returnedTokens = getTokensForCode(number)
    returnedTokens shouldEqual List(Token(TokenType.IntegerLiteral,
                                          number,
                                          CurrentPosition(0, 0)),
                                    eofToken(0, 1)).map(Some(_))
  }
  "!= in source" should "return correct number token" in {
    val code = "!="
    val returnedTokens = getTokensForCode(code)
    returnedTokens shouldEqual List(Token(TokenType.BangEqual,
                                          code,
                                          CurrentPosition(0, 0)),
                                    eofToken(0, 1)).map(Some(_))
  }
  "escaped string" should "return correct string token" in {
    val returnedTokens = getTokensForCode(escapedString)
    returnedTokens shouldEqual List(escapedStringToken, eofToken(0, 25))
      .map(Some(_))

  }
  "escaped string with comment" should "return correct string token" in {
    val returnedTokens = getTokensForCode(escapedStringWithComment)
    returnedTokens shouldEqual List(escapedStringToken, eofToken(0, 35))
      .map(Some(_))
  }
  "brackets" should "return correct list of tokens" in {
    val brackets: String =
      """
        |{{()
        |}}
        |""".stripMargin
    val bracketTokens: List[Token] = List(
      Token(TokenType.LeftBrace, "{", CurrentPosition(1, 0)),
      Token(TokenType.LeftBrace, "{", CurrentPosition(1, 1)),
      Token(TokenType.LeftParenthesis, "(", CurrentPosition(1, 2)),
      Token(TokenType.RightParenthesis, ")", CurrentPosition(1, 3)),
      Token(TokenType.RightBrace, "}", CurrentPosition(2, 0)),
      Token(TokenType.RightBrace, "}", CurrentPosition(2, 1)),
      eofToken(2, 2)
    )
    val returnedTokens = getTokensForCode(brackets)
    returnedTokens shouldEqual bracketTokens.map(Some(_))
  }
  "correct fun definition" should "return correct list of tokens" in {
    val fun: String = "def foo(): Int = "
    val funTokens: List[Token] = List(
      Token(TokenType.Fun, "def", CurrentPosition(0, 0)),
      Token(TokenType.Identifier, "foo", CurrentPosition(0, 4)),
      Token(TokenType.LeftParenthesis, "(", CurrentPosition(0, 7)),
      Token(TokenType.RightParenthesis, ")", CurrentPosition(0, 8)),
      Token(TokenType.Colon, ":", CurrentPosition(0, 9)),
      Token(TokenType.IntegerType, "Int", CurrentPosition(0, 11)),
      Token(TokenType.Assign, "=", CurrentPosition(0, 15)),
      eofToken(0, 16)
    )
    val returnedTokens = getTokensForCode(fun)
    returnedTokens shouldEqual funTokens.map(Some(_))
  }
  "correct for expression" should "return correct list of tokens" in {
    val forExp: String = "for (i <- 0 until 10)"
    val forExpTokens: List[Token] = List(
      Token(TokenType.For, "for", CurrentPosition(0, 0)),
      Token(TokenType.LeftParenthesis, "(", CurrentPosition(0, 4)),
      Token(TokenType.Identifier, "i", CurrentPosition(0, 5)),
      Token(TokenType.ForArrow, "<-", CurrentPosition(0, 7)),
      Token(TokenType.IntegerLiteral, "0", CurrentPosition(0, 10)),
      Token(TokenType.Until, "until", CurrentPosition(0, 12)),
      Token(TokenType.IntegerLiteral, "10", CurrentPosition(0, 18)),
      Token(TokenType.RightParenthesis, ")", CurrentPosition(0, 20)),
      eofToken(0, 20)
    )
    val returnedTokens = getTokensForCode(forExp)
    returnedTokens shouldEqual forExpTokens.map(Some(_))
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
    while (token.isDefined && token.get.tokenType != TokenType.Eof) {
      token = lexer.getNextToken
      tokens += token
    }
    tokens.toList
  }
  def eofToken(line: Int = 0, col: Int = 0): Token =
    Token(TokenType.Eof, EOF.toString, CurrentPosition(line, col))
}
