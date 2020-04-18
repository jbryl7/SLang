package slang.utils

import slang.utils.AnalysePhase.AnalysePhase

trait CustomException extends Exception {
  val analysePhase: AnalysePhase

}
