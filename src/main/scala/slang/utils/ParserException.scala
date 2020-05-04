package slang.utils

import slang.utils.AnalysePhase.AnalysePhase
import slang.utils.ParserExceptionType.ParserExceptionType

case class ParserException(exceptionType: ParserExceptionType)
    extends CustomException {

  val analysePhase: AnalysePhase = AnalysePhase.Parser
  val message: String = exceptionType match {
    case ParserExceptionType.InvalidExpression =>
      "Invalid expression"
  }
  override def toString: String = message
}

case object ParserExceptionType extends Enumeration {
  type ParserExceptionType = Value
  val InvalidExpression = Value
}
