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
  val maybeSource: Option[Source] = (path, code) match {
    case (Some(p), None) =>
      try {
        Some(Source.fromFile(p))
      } catch {
        case _: Exception =>
          val e = FileHandlerException(
            FileHandlerExceptionType.FileDoesNotExist)
          ExceptionHandler.reportException(e)
          None
      }

    case (None, Some(c)) => Some(Source.fromString(c))

    case _ =>
      val e = FileHandlerException(
        FileHandlerExceptionType.NoParametersProvided)
      ExceptionHandler.reportException(e)
      None
  }

  val source: Source = maybeSource.get
  val currentPosition: CurrentPosition = CurrentPosition(0, 0)
  var currentChar: Char = if (source.hasNext) source.next() else EOF

  def consumeChar(): Char = {
    val prevChar = currentChar
    if (source.hasNext) {
      currentChar = source.next()
      if (prevChar == '\n') {
        currentPosition.column = 0
        currentPosition.row += 1
      } else
        currentPosition.column += 1
    } else {
      currentChar = EOF
    }
    prevChar
  }

  def readChar: Char = currentChar
}

case class CurrentPosition(var row: Int, var column: Int)
