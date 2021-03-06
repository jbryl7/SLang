package slang.utils

import slang.utils.AnalysePhase.AnalysePhase
import slang.utils.LexerExceptionType.LexerExceptionType

case class LexerException(exceptionType: LexerExceptionType)
    extends CustomException {

  val analysePhase: AnalysePhase = AnalysePhase.Lexer
  val message: String = exceptionType match {
    case LexerExceptionType.TokenTypeNotFound => "Token type not found"
    case LexerExceptionType.InvalidSyntax     => "Invalid Syntax"
    case LexerExceptionType.IdentifierStartedWithDigit =>
      "Identifier started with digit"
  }
  override def toString: String = message
}

case object LexerExceptionType extends Enumeration {
  type LexerExceptionType = Value
  val InvalidSyntax, TokenTypeNotFound, IdentifierStartedWithDigit = Value
}
