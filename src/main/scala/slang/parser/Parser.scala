package slang.parser

import slang.instructions._
import slang.lexer.TokenType.TokenType
import slang.lexer._
import slang.utils._
import util.control.Breaks._
import scala.collection.mutable.ListBuffer

case class Parser(lexer: LexerInterface) {
  var maybeToken: Option[Token] = None
  var maybeParsedStatement: Option[Node] = None
  var currentToken: Token = _

  def parse(): Block = {
    maybeToken = lexer.getNextToken
    currentToken = maybeToken.get
    maybeParsedStatement = parseStatement()
    val instructions: ListBuffer[Node] = ListBuffer()
    breakable {
      while (maybeParsedStatement.isDefined) {
        instructions.append(maybeParsedStatement.get)
        println(currentToken)
        if (currentToken.tokenType == TokenType.Eof)
          break
        maybeParsedStatement = parseStatement()
      }
    }
    Block(instructions)
  }

  def parseStatement(): Option[Node] = {
    currentToken.tokenType match {
      case TokenType.Print      => parsePrint()
      case TokenType.Return     => parseReturn()
      case TokenType.Identifier => parseCall()
      case TokenType.Var        => parseVarDeclaration()
      case TokenType.Fun        => parseFunctionDeclaration()
      case TokenType.Class      => parseClassDeclaration()
      case TokenType.If         => parseIf()
      case _ =>
        ExceptionHandler.reportException(
          ParserException(ParserExceptionType.UnexpectedToken),
          Some(currentToken.toString))
        None
    }
  }

  def parseClassDeclaration(): Option[Node] = {
    accept(TokenType.Class)
    val identifier = currentToken.lexeme
    accept(TokenType.Identifier)
    val classBody = parseClassBody()

    Some(ClassDeclaration(identifier, classBody))
  }
  def parseClassBody(): ClassBody = {
    val varDeclarations: ListBuffer[VarDeclaration] = ListBuffer()
    val funDeclarations: ListBuffer[FunctionDeclaration] = ListBuffer()

    accept(TokenType.LeftBrace)

    while (currentToken.tokenType != TokenType.RightBrace) {
      currentToken.tokenType match {
        case TokenType.Var => varDeclarations.append(parseVarDeclaration().get)
        case TokenType.Fun =>
          funDeclarations.append(parseFunctionDeclaration().get)
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
  def parseFunctionDeclaration(): Option[FunctionDeclaration] = {
    accept(TokenType.Fun)
    val identifier = currentToken.lexeme
    accept(TokenType.Identifier)
    accept(TokenType.LeftParenthesis)
    val parameters = parseParameters()
    accept(TokenType.RightParenthesis)
    accept(TokenType.Colon)
    val returnType = parseType()
    accept(TokenType.Assign)
    val body = parseBlock()
    Some(FunctionDeclaration(identifier, returnType, parameters, body))
  }
  def parseBlock(): Block = {
    val instructions: ListBuffer[Node] = ListBuffer()
    if (currentToken.tokenType == TokenType.LeftBrace) {
      accept(TokenType.LeftBrace)
      while (currentToken.tokenType != TokenType.RightBrace) {
        if (currentToken.tokenType == TokenType.LeftBrace)
          instructions.append(parseBlock())
        else
          instructions.append(parseStatement().get)
      }
      accept(TokenType.RightBrace)
    } else
      instructions.append(parseStatement().get)

    Block(instructions)
  }

  def parseParameters(): ListBuffer[Parameter] = {
    val params: ListBuffer[Parameter] = ListBuffer()
    while (currentToken.tokenType != TokenType.RightParenthesis) {
      val identifier = currentToken.lexeme
      accept(TokenType.Identifier)
      accept(TokenType.Colon)
      val paramType = parseType()
      params.append(Parameter(identifier, paramType))
      if (currentToken.tokenType == TokenType.Comma)
        accept(TokenType.Comma)
    }
    params
  }

  def parseCall(): Option[Node] = {
    val identifier = currentToken.lexeme
    accept(TokenType.Identifier)
    currentToken.tokenType match {
      case TokenType.LeftParenthesis =>
        accept(TokenType.LeftParenthesis)
        val arguments = parseArguments()
        accept(TokenType.RightParenthesis)
        Some(FunctionCall(identifier, arguments))
      case TokenType.Dot =>
        accept(TokenType.Dot)
        val call = parseCall()
        Some(ObjectCall(identifier, call))
      case TokenType.Assign => parseAssignment(identifier)
      case _ =>
        Some(VariableCall(identifier))
    }
  }

  def parseAssignment(identifier: String) = {
    accept(TokenType.Assign)
    val expression = parseExpression()
    Some(VariableAssignment(VariableCall(identifier), expression))
  }

  def parseReturn(): Option[Node] = {
    accept(TokenType.Return)
    Some(Return(parseExpression()))
  }

  def parseIf(): Option[Node] = {
    accept(TokenType.If)
    accept(TokenType.LeftParenthesis)
    val condition = parseCondition()
    accept(TokenType.RightParenthesis)
    val ifBlock = parseBlock()
    val elseBlock: Option[Block] =
      if (currentToken.tokenType == TokenType.Else) {
        accept(TokenType.Else)
        Some(parseBlock())
      } else
        None

    Some(IfElse(condition, ifBlock, elseBlock))
  }

  def parseArguments(): ListBuffer[Node] = {
    val arguments: ListBuffer[Node] = ListBuffer();
    while (currentToken.tokenType != TokenType.RightParenthesis) {
      val argPossibleTypes = List(TokenType.IntegerLiteral,
                                  TokenType.StringLiteral,
                                  TokenType.Identifier,
                                  TokenType.Minus,
                                  TokenType.LeftParenthesis)

      currentToken.tokenType match {
        case t if argPossibleTypes.contains(t) =>
          arguments.append(parseExpression())
        case _ =>
          ExceptionHandler.reportException(
            ParserException(ParserExceptionType.InvalidExpression),
            Some(
              f"${currentToken.position} Expected one of ${argPossibleTypes}. Provided ${currentToken.tokenType}")
          )
      }
      if (currentToken.tokenType == TokenType.Comma)
        accept(TokenType.Comma)
    }
    arguments
  }

  def parseVarDeclaration(): Option[VarDeclaration] = {
    accept(TokenType.Var)
    val identifier = currentToken.lexeme
    accept(TokenType.Identifier)
    accept(TokenType.Colon)
    val varType = parseType()
    accept(TokenType.Assign)
    val expression = parseExpression()
    Some(new VarDeclaration(identifier, Some(expression), varType))
  }

  def parsePrint(): Option[Node] = {
    accept(TokenType.Print)
    accept(TokenType.LeftParenthesis)
    val expressionToPrint = parseExpression()
    accept(TokenType.RightParenthesis)
    Some(PrintCall(expressionToPrint))
  }
  def parseExpression(): Node = {

    val operands: ListBuffer[Node] = ListBuffer()
    val operators: ListBuffer[TokenType] = ListBuffer()

    if (TokenType.LeftBrace == currentToken.tokenType) // var x: Int = {x}
      ExceptionHandler.reportException(
        ParserException(ParserExceptionType.InvalidExpression),
        Some(s"${currentToken.position} found ${currentToken.lexeme}"))

    operands.append(parseMultiplicativeExpression())
    currentToken.tokenType match {
      case TokenType.Plus | TokenType.Minus =>
        operators.append(currentToken.tokenType)
        accept(currentToken.tokenType)
        operands.append(parseMultiplicativeExpression())
      case _ => ()
    }
    new Expression(operands, operators)
  }

  def parseMultiplicativeExpression(): Node = {

    val operands: ListBuffer[Node] = ListBuffer()
    val operators: ListBuffer[TokenType] = ListBuffer()
    operands.append(parsePrimaryExpression())
    while (List(TokenType.MultiplicativeOperator, TokenType.DivideOperator)
             .contains(currentToken.tokenType)) {
      operators.append(currentToken.tokenType)
      accept(List(TokenType.MultiplicativeOperator, TokenType.DivideOperator))
      operands.append(parsePrimaryExpression())
    }
    new Expression(operands, operators)
  }

  def parsePrimaryExpression(): Node = {
    val operands: ListBuffer[Node] = ListBuffer()
    val operators: ListBuffer[TokenType] = ListBuffer()
    currentToken.tokenType match {
      case TokenType.LeftParenthesis =>
        accept(TokenType.LeftParenthesis)
        operands.append(parseExpression())
        accept(TokenType.RightParenthesis)
      case TokenType.Identifier => operands.append(parseCall().get)
      case _                    => operands.append(parseLiteral())
    }
    new Expression(operands, operators)
  }

  def parseCondition(): Condition = {
    val operands: ListBuffer[Node] = ListBuffer()
    val operators: ListBuffer[TokenType] = ListBuffer()
    operands.append(parseAndCondition())
    while (currentToken.tokenType == TokenType.Or) {
      accept(TokenType.Or)
      operators.append(TokenType.Or)
      operands.append(parseAndCondition())
    }
    Condition(operands, operators)
  }
  def parseAndCondition(): Condition = {
    val operands: ListBuffer[Node] = ListBuffer()
    val operators: ListBuffer[TokenType] = ListBuffer()
    operands.append(parseEqualityCondition())
    while (currentToken.tokenType == TokenType.And) {
      accept(TokenType.And)
      operators.append(TokenType.And)
      operands.append(parseEqualityCondition())
    }
    Condition(operands, operators)
  }
  def parseEqualityCondition(): Condition = {
    val operands: ListBuffer[Node] = ListBuffer()
    val operators: ListBuffer[TokenType] = ListBuffer()
    operands.append(parseRelationalCondition())
    val equalityOperators = List(TokenType.Equal, TokenType.BangEqual)
    while (equalityOperators.contains(currentToken.tokenType)) {
      val tokenType = currentToken.tokenType
      accept(equalityOperators)
      operands.append(parseRelationalCondition())
      operators.append(tokenType)
    }
    Condition(operands, operators)
  }

  def parseRelationalCondition(): Condition = {
    val operands: ListBuffer[Node] = ListBuffer()
    val operators: ListBuffer[TokenType] = ListBuffer()
    operands.append(parsePrimaryCondition())
    val relationalOperators = List(TokenType.Less,
                                   TokenType.LessEqual,
                                   TokenType.Greater,
                                   TokenType.GreaterEqual)
    while (relationalOperators.contains(currentToken.tokenType)) {
      val tokenType = currentToken.tokenType
      accept(relationalOperators)
      operands.append(parsePrimaryCondition())
      operators.append(tokenType)
    }
    Condition(operands, operators)
  }

  def parsePrimaryCondition(): Condition = {
    val operands: ListBuffer[Node] = ListBuffer()
    val operators: ListBuffer[TokenType] = ListBuffer()
    if (currentToken.tokenType == TokenType.LeftParenthesis) {
      accept(TokenType.LeftParenthesis)
      operands.append(parseCondition())
      accept(TokenType.RightParenthesis)
    } else
      operands.append(parseExpression())

    Condition(operands, operators)
  }

  def parseNumber(sign: Int = 1): MyInt = {
    currentToken.tokenType match {
      case TokenType.IntegerLiteral =>
        val myInt = MyInt(currentToken.lexeme.toInt * sign)
        accept(TokenType.IntegerLiteral)
        myInt
      case _ => println("something impossible happened"); MyInt()
    }
  }

  def parseType(): TokenType = {
    val currentType = currentToken.tokenType
    accept(
      List(TokenType.UnitType,
           TokenType.IntegerType,
           TokenType.StringType,
           TokenType.Identifier))
    currentType
  }
  def parseLiteral(): Node = {
    currentToken.tokenType match {
      case TokenType.Minus =>
        accept(TokenType.Minus)
        parseNumber(-1)
      case TokenType.Plus =>
        accept(TokenType.Plus)
        parseNumber()
      case TokenType.IntegerLiteral =>
        parseNumber()
      case TokenType.StringLiteral =>
        val ret = MyString(currentToken.lexeme)
        accept(TokenType.StringLiteral)
        ret
      case _ => println("something impossible happened"); MyString()
    }
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
