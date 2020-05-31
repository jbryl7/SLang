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
    case MyRuntimeExceptionType.MinusNotANumber =>
      "Invalid expression. MINUS <not a number>"
    case MyRuntimeExceptionType.SumIncompatibleTypes =>
      "You can not sum up these types"
    case MyRuntimeExceptionType.UndefinedVariable =>
      "Variable u requested is not defined"
    case MyRuntimeExceptionType.UndefinedAttribute =>
      "invalid attribute"
    case e => f"implement error message for ${e}"
  }
  override def toString: String = message
}

case object MyRuntimeExceptionType extends Enumeration {
  type MyRuntimeExceptionType = Value
  val MinusNotANumber, OperandMustBeANumber, SumIncompatibleTypes,
  IdentifierNotInScope, AlreadyDeclared, InvalidArgumentException,
  UndefinedVariable, UndefinedAttribute, InvalidNumberOfArguments =
    Value

}
