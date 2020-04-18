package slang.utils

import slang.utils.AnalysePhase.AnalysePhase
import slang.utils.FileHandlerExceptionType.FileHandlerExceptionType

case class FileHandlerException(exceptionType: FileHandlerExceptionType)
    extends CustomException {

  val analysePhase: AnalysePhase = AnalysePhase.FileHandler
  val message: String = exceptionType match {
    case FileHandlerExceptionType.FileDoesNotExist => "File does not exit"
    case FileHandlerExceptionType.NoParametersProvided =>
      "No parameters provided"
  }
  override def toString: String = message
}

case object FileHandlerExceptionType extends Enumeration {
  type FileHandlerExceptionType = Value
  val FileDoesNotExist, NoParametersProvided = Value
}
