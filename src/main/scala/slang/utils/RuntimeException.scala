package slang.utils

import slang.utils.AnalysePhase.AnalysePhase
import slang.utils.RuntimeExceptionType.RuntimeExceptionType

case class RuntimeException(exceptionType: RuntimeExceptionType)
    extends CustomException {

  val analysePhase: AnalysePhase = AnalysePhase.Runtime
  val message: String = exceptionType match {
    case RuntimeExceptionType.InvalidArgumentException =>
      "Invalid argument type"
    case RuntimeExceptionType.InvalidNumberOfArguments =>
      "Invalid number of arguments"
  }
  override def toString: String = message
}

case object RuntimeExceptionType extends Enumeration {
  type RuntimeExceptionType = Value
  val InvalidArgumentException, InvalidNumberOfArguments = Value
}
