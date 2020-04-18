package slang.utils

import slang.utils.AnalysePhase.AnalysePhase

object ExceptionHandler {
  def reportException(e: CustomException,
                      additionalInfo: Option[String] = None): Unit = {
    println(s"${e.analysePhase}: ${e} ${additionalInfo.getOrElse("")}")
    throw (e)
  }
}
