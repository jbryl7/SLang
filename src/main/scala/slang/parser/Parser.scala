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
  val program: Program = Program(
    lexer.fileHandler.path.getOrElse("DefaultName"))
  var currentScope: Scope = program.currentScope

  def parse(): Program = {
    maybeToken = lexer.getNextToken
    currentToken = maybeToken.get
    currentScope = program.getCurrentScope()
    maybeParsedStatement = parseInstruction(currentScope)
    breakable {
      while (maybeParsedStatement.isDefined) {
        program.addStatement(maybeParsedStatement.get)
        println(maybeParsedStatement)
        if (currentToken.tokenType == TokenType.Eof)
          break
        maybeParsedStatement = parseInstruction(currentScope)
      }
    }
    program
  }

  def parseClassDeclaration(scope: Scope): Option[Node] = ???

  def parseInstruction(scope: Scope): Option[Node] = {
    println("statement")
    maybeToken match {
      case Some(Token(TokenType.LeftBrace, _, _))  => openNewScope()
      case Some(Token(TokenType.Identifier, _, _)) => parseCall()
      case Some(Token(TokenType.If, _, _))         => parseIf(scope)
      case Some(Token(TokenType.Fun, _, _))        => parseFunctionDeclaration(scope)
      case Some(Token(TokenType.Var, _, _))        => parseVarDeclaration(scope)
      case Some(Token(TokenType.Class, _, _))      => parseClassDeclaration(scope)
      case Some(Token(TokenType.Return, _, _))     => parseReturn(scope)
      case _ =>
        ExceptionHandler.reportException(
          ParserException(ParserExceptionType.InvalidExpression),
          Some(
            f"${currentToken.position} expected: ${List(TokenType.LeftBrace, TokenType.Identifier, TokenType.If, TokenType.Fun, TokenType.Var, TokenType.Return)} provided: ${currentToken.tokenType}")
        )
        None
    }
  }

  def openNewScope(): Option[Node] = {
    println("new scope")
    program.addLocalScope()
    currentScope = program.addLocalScope()
    parseInstruction(currentScope)
  }

  def parseCall(): Option[Node] = {
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
        objectCall.call = parseCall()
        Some(objectCall)
      case TokenType.Eof =>
        ExceptionHandler.reportException(
          ParserException(ParserExceptionType.UnexpectedEOF))
        None
      case _ =>
        println("varCall")
        println(currentToken)
        Some(VariableCall(identifier)) //to be corrected
    }
  }

  def parseClassCall(scope: Scope): Option[Node] = {
    println("class call")
    None
  }

  def parseReturn(scope: Scope): Option[Node] = {
    println("return")
    accept(TokenType.Return)
    Some(parseExpression())
  }

  def parseVarDeclaration(scope: Scope): Option[Node] = {
    println("var declaration")
    accept(TokenType.Var)
    val identifier = currentToken.lexeme
    accept(TokenType.Identifier)
    accept(TokenType.Colon)
    val varType = parseType() // Identifier for class type
    accept(TokenType.Assign)
    val expression = parseExpression()

    Some(VarDeclaration(identifier, expression, varType))
  }

  def parseIf(scope: Scope): Option[Node] = {
    println("if")
    accept(TokenType.If)
    accept(TokenType.LeftParenthesis)

    val condition = parseCondition()
    accept(TokenType.RightParenthesis)
    val ifBlock = parseFunctionBody(scope)
    var foundElse = false
    var elseBlock: Option[Block] = None
    if (currentToken.tokenType == TokenType.Else) {
      foundElse = true
      accept(TokenType.Else)
      elseBlock = Some(parseFunctionBody(scope))
    }
    Some(IfElse(condition, ifBlock, elseBlock))
  }

  def parseArguments(): ListBuffer[Node] = {
    println("args")
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

  def parseParameters(): ListBuffer[Parameter] = {
    println("params")
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

  def parseFunctionDeclaration(scope: Scope): Option[Node] = {
    println("fun")
    accept(TokenType.Fun)
    val identifier = currentToken.lexeme
    accept(TokenType.Identifier)
    accept(TokenType.LeftParenthesis)
    val params = parseParameters()
    accept(TokenType.RightParenthesis)
    accept(TokenType.Colon)
    val funType = parseType()
    accept(TokenType.Assign)
    val body = parseFunctionBody(scope)
    Some(FunctionDeclaration(identifier, funType, body, params))
  }

  def parseFunctionBody(scope: Scope): Block = {
    println("fun body")
    val block = Block()
    var endOfBlock = true
    if (currentToken.tokenType == TokenType.LeftBrace) {
      accept(TokenType.LeftBrace)
      endOfBlock = false
    }
    do {
      currentToken.tokenType match {
        case TokenType.If | TokenType.Fun | TokenType.Identifier |
            TokenType.Class | TokenType.Return | TokenType.Var =>
          block.addInstruction(parseInstruction(scope).get)
        case TokenType.RightBrace =>
          accept(TokenType.RightBrace)
          endOfBlock = true
        case TokenType.Eof =>
          ExceptionHandler.reportException(
            ParserException(ParserExceptionType.UnexpectedEOF))
      }
    } while (!endOfBlock)

    block
  }

  def parseExpression(): Node = {
    println("expression")
    val expression = new Expression()
    expression.addOperand(parseMultiplicativeExpression())
    currentToken.tokenType match {
      case TokenType.Plus | TokenType.Minus =>
        expression.addOperator(currentToken.tokenType)
        accept(currentToken.tokenType)
        expression.addOperand(parseMultiplicativeExpression())
      case _ => ()
    }
    expression
  }

  def parseMultiplicativeExpression(): Node = {
    println("multiplicative exp")
    val expression = new Expression()
    expression.addOperand(parsePrimaryExpression())
    while (List(TokenType.MultiplicativeOperator, TokenType.DivideOperator)
             .contains(currentToken.tokenType)) {
      expression.addOperator(currentToken.tokenType)
      accept(List(TokenType.MultiplicativeOperator, TokenType.DivideOperator))
      expression.addOperand(parsePrimaryExpression())
    }
    expression
  }

  def parsePrimaryExpression(): Node = {
    println("primary exp")
    val expression = new Expression()
    currentToken.tokenType match {
      case TokenType.LeftParenthesis =>
        accept(TokenType.LeftParenthesis)
        expression.addOperand(parseExpression())
        accept(TokenType.RightParenthesis)
      case TokenType.Identifier => expression.addOperand(parseCall().get)
      case _                    => expression.addOperand(parseLiteral())
    }
    expression
  }

  def parseCondition(): Condition = {
    println("condition")
    val condition = Condition()
    condition.addOperand(parseAndCondition())
    while (currentToken.tokenType == TokenType.Or) {
      accept(TokenType.Or)
      condition.addOperator(TokenType.Or)
      condition.addOperand(parseAndCondition())
    }
    condition
  }
  def parseAndCondition(): Condition = {
    println("condition")
    val condition = Condition()
    condition.addOperand(parseEqualityCondition())
    while (currentToken.tokenType == TokenType.And) {
      accept(TokenType.And)
      condition.addOperator(TokenType.And)
      condition.addOperand(parseEqualityCondition())
    }
    condition
  }
  def parseEqualityCondition(): Condition = {
    println("equalityCondition")
    val condition = Condition()
    condition.addOperand(parseRelationalCondition())
    val equalityOperators = List(TokenType.Equal, TokenType.BangEqual)
    while (equalityOperators.contains(currentToken.tokenType)) {
      val tokenType = currentToken.tokenType
      accept(equalityOperators)
      condition.addOperand(parseRelationalCondition())
      condition.addOperator(tokenType)
    }
    condition
  }

  def parseRelationalCondition(): Condition = {
    println("relationalCondition")
    val condition = Condition()
    condition.addOperand(parsePrimaryCondition())
    val relationalOperators = List(TokenType.Less,
                                   TokenType.LessEqual,
                                   TokenType.Greater,
                                   TokenType.GreaterEqual)
    while (relationalOperators.contains(currentToken.tokenType)) {
      val tokenType = currentToken.tokenType
      accept(relationalOperators)
      condition.addOperand(parsePrimaryCondition())
      condition.addOperator(tokenType)
    }
    condition
  }

  def parsePrimaryCondition(): Condition = {
    println("primaryCondition")
    val condition = Condition()
    if (currentToken.tokenType == TokenType.LeftParenthesis) {
      accept(TokenType.LeftParenthesis)
      condition.addOperand(parseCondition())
      accept(TokenType.RightParenthesis)
    } else
      condition.addOperand(parseExpression())

    condition
  }

  def parseNumber(sign: Int = 1): MyInt = {
    println("number")
    currentToken.tokenType match {
      case TokenType.IntegerLiteral =>
        val myInt = MyInt(currentToken.lexeme.toInt * sign)
        accept(TokenType.IntegerLiteral)
        myInt
      case _ => println("numberProblem"); MyInt()
    }
  }

  def parseType(): TokenType = {
    println("type")
    val currentType = currentToken.tokenType
    accept(
      List(TokenType.UnitType,
           TokenType.IntegerType,
           TokenType.StringType,
           TokenType.Identifier))
    currentType
  }
  def parseLiteral(): Node = {
    println("literal")
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
      case _ => println("literalProblem"); MyString()
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
