package slang.utils

import slang.utils.AnalysePhase.AnalysePhase
import slang.utils.ParserExceptionType.ParserExceptionType

case class ParserException(exceptionType: ParserExceptionType)
    extends CustomException {

  val analysePhase: AnalysePhase = AnalysePhase.Parser
  val message: String = exceptionType match {
    case ParserExceptionType.InvalidExpression =>
      "Invalid expression"
    case ParserExceptionType.DotWithoutIdentifier =>
      "Expected identifier after object."
    case ParserExceptionType.UnexpectedEOF =>
      "Unexpected end of file."
    case ParserExceptionType.IdentifierAlreadyInScope =>
      "Identifier already declared in scope."
  }
  override def toString: String = message
}

case object ParserExceptionType extends Enumeration {
  type ParserExceptionType = Value
  val InvalidExpression, UnexpectedEOF, DotWithoutIdentifier,
  IdentifierAlreadyInScope = Value

}
