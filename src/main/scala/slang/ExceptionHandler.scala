package slang

import AnalysePhase.AnalysePhase

object ExceptionHandler {
  def logException(message: String): Unit = println(message)
  def reportException(phase: AnalysePhase,
                      e: Exception,
                      additionalInfo: Option[String] = None): Unit = {
    logException(s"${phase}: ${e} ${additionalInfo.getOrElse("")}")
  }
}
