package slang.parser

import slang.instructions._
import slang.lexer.TokenType.TokenType
import slang.lexer._
import slang.utils._

import scala.collection.mutable.ListBuffer

case class Parser(lexer: Lexer) {
  var maybeToken: Option[Token] = None
  var maybeParsedStatement: Option[Node] = None
  var currentToken: Token = _
  val program: Program = Program(
    lexer.fileHandler.path.getOrElse("DefaultName"))
  var currentScope: Scope = program.currentScope

  def parse(): Program = {
    maybeToken = lexer.getNextToken
    currentToken = maybeToken.get
    currentScope = program.getCurrentScope()
    maybeParsedStatement = parseStatement(currentScope)

    while (maybeParsedStatement.isDefined) {
      program.addStatement(maybeParsedStatement.get)
      println(maybeParsedStatement)
      maybeParsedStatement = parseStatement(currentScope)
    }
    program
  }

  def parseStatement(scope: Scope): Option[Node] = {
    maybeToken match {
      case Some(Token(TokenType.LeftBrace, _, _))  => openNewScope()
      case Some(Token(TokenType.Identifier, _, _)) => parseCall(scope)
      case Some(Token(TokenType.If, _, _))         => parseIf(scope)
      case Some(Token(TokenType.Fun, _, _))        => parseFunctionDeclaration(scope)
      case Some(Token(TokenType.Var, _, _))        => parseVarDeclaration(scope)
      case Some(Token(TokenType.Return, _, _))     => parseReturn(scope)
      case _ =>
        ExceptionHandler.reportException(
          ParserException(ParserExceptionType.InvalidExpression),
          Some(
            f"${currentToken.position} expected: xd provided: ${currentToken.tokenType}"))
        None
    }
  }
  def openNewScope(): Option[Node] = {
    println("new scope")
    program.addLocalScope()
    currentScope = program.addLocalScope()
    parseStatement(currentScope)
  }

  def parseCall(scope: Scope): Option[Node] = {
    println("just call")
    val identifier = currentToken.lexeme

    accept(TokenType.Identifier)
    currentToken.tokenType match {
      case TokenType.LeftParenthesis =>
        println("funCall")
        val funCall = FunctionCall(currentToken.lexeme)
        accept(TokenType.LeftParenthesis)
        funCall.arguments = parseArguments()
        accept(TokenType.RightParenthesis)
        Some(funCall)
      case TokenType.Dot => // todo correct this, to use object scope
        println("object member call")
        accept(TokenType.Dot)
        val objectCall = ObjectCall(identifier)
        objectCall.call = parseCall(scope)
        Some(objectCall)
      case _ =>
        println("varCall")
        Some(VariableCall(identifier)) //to be corrected
    }
  }
  def parseClassCall(scope: Scope): Option[Node] = {
    println("class call")
    ???
  }
  def parseReturn(scope: Scope): Option[Node] = {
    println("return")
    accept(TokenType.Return)
    parseExpression()
  }
  def parseVarDeclaration(scope: Scope): Option[Node] = {
    println("var declaration")
    val varDeclaration = VarDeclaration(currentToken.lexeme)
    accept(TokenType.Var)
    val identifier = currentToken.lexeme
    accept(TokenType.Identifier)
    val varType = currentToken.lexeme
    accept(TokenType.Colon)
    accept(List(TokenType.Type, TokenType.Identifier)) // Identifier for class type
    accept(TokenType.Assign)
    parseExpression()
    Some(varDeclaration)
  }
  def parseIf(scope: Scope): Option[Node] = {
    println("if")
    accept(TokenType.LeftParenthesis)
    parseCondition(scope)
  }
  def parseCondition(scope: Scope): Option[Node] = {
    println("condition")
    ???
  }
  def parseArguments() = {
    println("args")
    val arguments: ListBuffer[Node] = ListBuffer();
    while (currentToken.tokenType != TokenType.RightParenthesis) {
      val argPossibleTypes = List(TokenType.Type,
                                  TokenType.Identifier,
                                  TokenType.Minus,
                                  TokenType.LeftParenthesis)

      currentToken.tokenType match {
        case t if argPossibleTypes.contains(t) =>
          arguments.append(parseExpression().get)
        case _ =>
          ExceptionHandler.reportException(
            ParserException(ParserExceptionType.InvalidExpression),
            Some(
              f"${currentToken.position} Expected one of ${argPossibleTypes}. Provided ${currentToken.tokenType}")
          )
      }
      accept(TokenType.Comma)
    }
    arguments
  }
  def parseFunctionDeclaration(scope: Scope): Option[Node] = {
    println("fun declaration")
    None
  }
  def parseFunctionBody(scope: Scope) = {
    println("fun body")
    ???
  }
  def accept(tokenType: TokenType): Boolean = accept(List(tokenType))

  def accept(tokenTypes: List[TokenType]): Boolean =
    if (tokenTypes.contains(currentToken.tokenType)) {
      maybeToken = lexer.getNextToken;
      if (maybeToken.isEmpty) // unnecessary check
        ExceptionHandler.reportException(
          ParserException(ParserExceptionType.InvalidExpression))
      currentToken = maybeToken.get
      true;
    } else {
      ExceptionHandler.reportException(
        ParserException(ParserExceptionType.InvalidExpression),
        Some(f"${currentToken.position} expected ${tokenTypes.size match {
          case 1 => tokenTypes.head
          case _ => s"one of ${tokenTypes}"
        }}. Provided ${currentToken.tokenType}")
      )
      false
    }
  def parseExpression(): Option[Node] = {
    println("expression")
    None
  }

}
