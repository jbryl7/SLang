package slang.utils

import slang.utils.AnalysePhase.AnalysePhase
import slang.utils.MyRuntimeExceptionType.MyRuntimeExceptionType

case class MyRuntimeException(exceptionType: MyRuntimeExceptionType)
    extends CustomException {

  val analysePhase: AnalysePhase = AnalysePhase.Runtime
  import MyRuntimeExceptionType._
  val message: String = exceptionType match {
    case IdentifierNotInScope =>
      "Identifier not in scope"
    case InvalidNumberOfArguments =>
      "invalid number of arguments"
    case InvalidArgumentException =>
      "Invalid argument provided"
    case MinusNotANumber =>
      "Invalid expression. MINUS <not a number>"
    case SumIncompatibleTypes =>
      "You can not sum up these types"
    case UndefinedVariable =>
      "Variable u requested is not defined"
    case UndefinedAttribute =>
      "invalid attribute"
    case YouCanCallOnlyFunctionsAndClasses =>
      "You can call only functions and classes"
    case e => f"implement error message for ${e}"
  }
  override def toString: String = message
}

case object MyRuntimeExceptionType extends Enumeration {
  type MyRuntimeExceptionType = Value
  val MinusNotANumber, OperandMustBeANumber, SumIncompatibleTypes,
  IdentifierNotInScope, YouCanCallOnlyFunctionsAndClasses, AlreadyDeclared,
  InvalidArgumentException, UndefinedVariable, UndefinedAttribute,
  InvalidNumberOfArguments =
    Value

}
