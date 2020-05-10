package slang.utils

import slang.utils.AnalysePhase.AnalysePhase
import slang.utils.MyRuntimeExceptionType.MyRuntimeExceptionType

case class MyRuntimeException(exceptionType: MyRuntimeExceptionType)
    extends CustomException {

  val analysePhase: AnalysePhase = AnalysePhase.Runtime
  val message: String = exceptionType match {
    case MyRuntimeExceptionType.IdentifierNotInScope =>
      "Identifier not in scope"
    case MyRuntimeExceptionType.InvalidNumberOfArguments =>
      "invalid number of arguments"
    case MyRuntimeExceptionType.InvalidArgumentException =>
      "Invalid argument provided"

  }
  override def toString: String = message
}

case object MyRuntimeExceptionType extends Enumeration {
  type MyRuntimeExceptionType = Value
  val IdentifierNotInScope, InvalidArgumentException, InvalidNumberOfArguments =
    Value

}