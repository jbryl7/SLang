package slang.lexer
import ReaderExceptionType.ReaderExceptionType

case class ReaderException(exceptionType: ReaderExceptionType)
    extends Exception {

  val message: String = exceptionType match {
    case ReaderExceptionType.FileDoesNotExist     => "File does not exit"
    case ReaderExceptionType.NoParametersProvided => "No parameters provided"
  }
  override def toString: String = message
}

case object ReaderExceptionType extends Enumeration {
  type ReaderExceptionType = Value
  val FileDoesNotExist, NoParametersProvided = Value

}
