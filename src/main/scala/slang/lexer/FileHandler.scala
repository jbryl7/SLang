package slang.lexer

import scala.io.Source
import slang._
import slang.utils.{
  AnalysePhase,
  ExceptionHandler,
  FileHandlerException,
  FileHandlerExceptionType
}

case class FileHandler(path: Option[String] = None,
                       code: Option[String] = None) {
  val maybeSource: Option[String] = (path, code) match {
    case (Some(p), None) =>
      try {
        val file = Source.fromFile(p)
        val code = file.getLines.toList.mkString("\n")
        file.close()
        Some(code)
      } catch {
        case _: Exception =>
          val e = FileHandlerException(
            FileHandlerExceptionType.FileDoesNotExist)
          ExceptionHandler.reportException(e)
          None
      }
    case (None, Some(c)) => Some(c)
    case _ =>
      val e = FileHandlerException(
        FileHandlerExceptionType.NoParametersProvided)
      ExceptionHandler.reportException(e)
      None
  }

  val source: String = maybeSource.getOrElse("")
  val sourceSize: Int = source.length
  var currentChar: Char = if (sourceSize > 0) source(0) else EOF
  val currentPosition: CurrentPosition = CurrentPosition(0, 0)
  var pos = 1

  def readChar: Char =
    currentChar

  def consumeChar: Char = {
    val oldChar = currentChar
    if (pos == sourceSize) {
      pos += 1
      currentChar = EOF
      return oldChar
    }
    if (pos > sourceSize) {
      currentChar = EOF
      return currentChar
    }
    currentChar = source(pos)
    if (oldChar == '\n') {
      currentPosition.row += 1
      currentPosition.column = 0
    } else
      currentPosition.column += 1
    pos += 1
    oldChar
  }
}
case class CurrentPosition(var row: Int, var column: Int) {
  def apply(currentPosition: CurrentPosition): CurrentPosition =
    CurrentPosition(currentPosition.row, currentPosition.column)
}
