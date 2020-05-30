package slang.parser
import slang.instructions._
import slang.instructions.customtypes.MyString
import slang.instructions.expressions._
import slang.instructions.statements._
import slang.lexer.TokenType.TokenType
import slang.lexer._
import slang.utils._

import util.control.Breaks._
import scala.collection.mutable.ListBuffer

case class Parser(lexer: LexerInterface) {
  var maybeToken: Option[Token] = None
  var parsedStatement: Statement = null
  var currentToken: Token = _

  def parse(): Block = {
    maybeToken = lexer.getNextToken
    currentToken = maybeToken.get
    parsedStatement = parseStatement()
    val instructions: ListBuffer[Statement] = ListBuffer()
    breakable {
      while (parsedStatement != null) {
        instructions.append(parsedStatement)
        if (currentToken.tokenType == TokenType.Eof)
          break
        parsedStatement = parseStatement()
      }
    }
    Block(instructions)
  }

  def parseStatement(): Statement = {
    currentToken.tokenType match {
      case TokenType.Print      => parsePrint()
      case TokenType.Return     => parseReturn()
      case TokenType.Identifier => ExpressionStatement(parseAssignment)
      case TokenType.Var        => parseVarDeclaration()
      case TokenType.Fun        => parseFunctionDeclaration()
      case TokenType.Class      => parseClassDeclaration()
      case TokenType.If         => parseIf()
      case _ =>
        ExceptionHandler.reportException(
          ParserException(ParserExceptionType.UnexpectedToken),
          Some(currentToken.toString))
        null
    }
  }

  def parseClassDeclaration(): Statement = {
    accept(TokenType.Class)
    val identifier = currentToken
    accept(TokenType.Identifier)
    val classBody = parseClassBody()

    ClassStatement(identifier, classBody)
  }
  def parseClassBody(): ClassBody = {
    val varDeclarations: ListBuffer[VarStatement] = ListBuffer()
    val funDeclarations: ListBuffer[FunctionStatement] = ListBuffer()

    accept(TokenType.LeftBrace)

    while (currentToken.tokenType != TokenType.RightBrace) {
      currentToken.tokenType match {
        case TokenType.Var => varDeclarations.append(parseVarDeclaration())
        case TokenType.Fun =>
          funDeclarations.append(parseFunctionDeclaration())
        case _ =>
          ExceptionHandler.reportException(
            ParserException(
              ParserExceptionType.InvalidExpressionInClassDeclaration),
            Some(currentToken.toString))
      }
    }
    accept(TokenType.RightBrace)

    ClassBody(varDeclarations, funDeclarations)
  }
  def parseFunctionDeclaration(): FunctionStatement = {
    accept(TokenType.Fun)
    val identifier = currentToken
    accept(TokenType.Identifier)
    accept(TokenType.LeftParenthesis)
    val parameters = parseParameters()
    accept(TokenType.RightParenthesis)
    accept(TokenType.Colon)
    val returnType = parseType()
    accept(TokenType.Assign)
    val body = parseBlock()
    FunctionStatement(identifier, returnType, parameters, body)
  }
  def parseBlock(): Block = {
    val instructions: ListBuffer[Statement] = ListBuffer()
    if (currentToken.tokenType == TokenType.LeftBrace) {
      accept(TokenType.LeftBrace)
      while (currentToken.tokenType != TokenType.RightBrace) {
        if (currentToken.tokenType == TokenType.LeftBrace)
          instructions.append(parseBlock())
        else
          instructions.append(parseStatement())
      }
      accept(TokenType.RightBrace)
    } else
      instructions.append(parseStatement())

    Block(instructions)
  }

  def parseParameters(): ListBuffer[Parameter] = {
    val params: ListBuffer[Parameter] = ListBuffer()
    while (currentToken.tokenType != TokenType.RightParenthesis) {
      val identifier = currentToken
      accept(TokenType.Identifier)
      accept(TokenType.Colon)
      val paramType = parseType()
      params.append(Parameter(identifier, paramType))
      if (currentToken.tokenType == TokenType.Comma)
        accept(TokenType.Comma)
    }
    params
  }

  def parseReturn(): Statement = {
    val tok = currentToken
    accept(TokenType.Return)
    var expr: Expression = null
    if (currentToken.position.row == tok.position.row && currentToken.tokenType != TokenType.RightBrace)
      expr = parseExpression()
    ReturnStatement(tok, expr)
  }

  def parseIf(): Statement = {
    accept(TokenType.If)
    accept(TokenType.LeftParenthesis)
    val condition = parseExpression()
    accept(TokenType.RightParenthesis)
    val ifBlock = parseBlock()
    val elseBlock: Block =
      if (currentToken.tokenType == TokenType.Else) {
        accept(TokenType.Else)
        parseBlock()
      } else null
    IfStatement(condition, ifBlock, elseBlock)
  }

  def parseVarDeclaration(): VarStatement = {
    accept(TokenType.Var)
    val identifier = currentToken
    accept(TokenType.Identifier)
    accept(TokenType.Colon)
    val varType = parseType()
    accept(TokenType.Assign)
    val expression = parseExpression()
    VarStatement(identifier, expression, varType)
  }

  def parsePrint(): Statement = {
    accept(TokenType.Print)
    accept(TokenType.LeftParenthesis)
    val expressionToPrint = parseExpression()
    accept(TokenType.RightParenthesis)
    PrintStatement(expressionToPrint)
  }
  def parseExpression(): Expression = {
    parseAssignment
  }

  private def parseAssignment: Expression = {
    var expr: Expression = parseOr
    if (currentToken.tokenType == TokenType.Assign) {
      print("entered")
      accept(TokenType.Assign)
      val value = parseAssignment
      expr match {
        case e: VariableExpression =>
          expr = AssignExpression(e.name, value)
        case e: GetExpression =>
          expr = SetExpression(e.`object`, e.name, value)
      }
    }
    expr
  }

  private def parseOr = {
    var expr = parseAnd
    while ({ currentToken.tokenType == TokenType.Or }) {
      val operator = currentToken
      accept(currentToken.tokenType)
      val right = parseAnd

      expr = LogicalExpression(expr, operator, right)
    }
    expr
  }

  private def parseAnd = {
    var expr = parseEquality
    while (currentToken.tokenType == TokenType.And) {
      val operator = currentToken
      accept(currentToken.tokenType)
      val right = parseEquality
      expr = LogicalExpression(expr, operator, right)
    }
    expr
  }

  private def parseEquality = {
    var expr = parseComparison
    while (currentToken.tokenType == TokenType.BangEqual || currentToken.tokenType == TokenType.Equal) {
      val operator = currentToken
      accept(currentToken.tokenType)
      val right = parseComparison
      expr = BinaryExpression(expr, operator, right)
    }
    expr
  }

  private def parseComparison = {
    var expr = parseAddition
    val comparisonOperators = List(TokenType.Greater,
                                   TokenType.GreaterEqual,
                                   TokenType.Less,
                                   TokenType.LessEqual)
    while ({
      comparisonOperators.contains(currentToken.tokenType)
    }) {
      val operator = currentToken
      accept(currentToken.tokenType)
      val right = parseAddition
      expr = BinaryExpression(expr, operator, right)
    }
    expr
  }

  private def parseAddition = {
    var expr = parseMultiplication
    while (currentToken.tokenType == TokenType.Plus || currentToken.tokenType == TokenType.Minus) {
      val operator = currentToken
      accept(currentToken.tokenType)
      val right = parseMultiplication
      expr = BinaryExpression(expr, operator, right)
    }
    expr
  }

  private def parseMultiplication = {
    var expr = parseUnary
    while (currentToken.tokenType == TokenType.MultiplicativeOperator || currentToken.tokenType == TokenType.DivideOperator) {
      val operator = currentToken
      accept(currentToken.tokenType)
      val right = parseUnary
      expr = BinaryExpression(expr, operator, right)
    }
    expr
  }

  //< addition-and-multiplication
  //> unary
  private def parseUnary: Expression =
    if (currentToken.tokenType == TokenType.Minus || currentToken.tokenType == TokenType.Bang) {
      val operator = currentToken
      accept(currentToken.tokenType)
      val right = parseUnary
      UnaryExpression(operator, right)

    } else parseCall

  private def finishCall(callee: Expression): Expression = {

    accept(TokenType.LeftParenthesis)

    val paren = currentToken
    val arguments = parseArguments()
    accept(TokenType.RightParenthesis)
    CallExpression(callee, paren, arguments)
  }

  def parseArguments(): ListBuffer[Expression] = {
    def acceptIfComma: Boolean = {
      val ret = currentToken.tokenType == TokenType.Comma
      if (ret)
        accept(TokenType.Comma)
      ret
    }

    val arguments: ListBuffer[Expression] = ListBuffer()
    if (currentToken.tokenType != TokenType.RightParenthesis)
      do arguments.append(parseExpression()) while (acceptIfComma)
    arguments
  }

  private def parseCall(): Expression = {
    var expr = parsePrimaryExpression
    breakable {
      while (true) {
        if (currentToken.tokenType == TokenType.LeftParenthesis)
          expr = finishCall(expr)
        else if (currentToken.tokenType == TokenType.Dot) {
          accept(TokenType.Dot)

          val name = currentToken
          accept(TokenType.Identifier)
          expr = GetExpression(expr, name)
        } else break
      }
    }

    expr
  }

  private def parsePrimaryExpression: Expression = {
    currentToken.tokenType match {
      case TokenType.False =>
        accept(TokenType.False)
        LiteralExpression(false)

      case TokenType.True =>
        accept(TokenType.True)
        LiteralExpression(true)

      case TokenType.IntegerLiteral =>
        val ret = LiteralExpression(currentToken.lexeme.toInt)
        accept(TokenType.IntegerLiteral)
        ret
      case TokenType.StringLiteral =>
        val ret = LiteralExpression(currentToken.lexeme)
        accept(TokenType.StringLiteral)
        ret
      case TokenType.This =>
        val ret = ThisExpression(currentToken)
        accept(TokenType.This)
        ret
      case TokenType.Identifier =>
        val ret = VariableExpression(currentToken)
        accept(TokenType.Identifier)
        ret
      case TokenType.LeftParenthesis =>
        accept(TokenType.LeftParenthesis)
        val expr = parseExpression()
        accept(TokenType.RightParenthesis)
        GroupingExpression(expr)
      case _ =>
        ExceptionHandler.reportException(
          ParserException(ParserExceptionType.InvalidExpression),
          Some(currentToken.toString))
        LiteralExpression(0)
    }
  }

  def parseType(): Token = { //todo
    val currentType = currentToken
    accept(
      List(TokenType.UnitType,
           TokenType.IntegerType,
           TokenType.StringType,
           TokenType.Identifier))
    currentType
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

}
