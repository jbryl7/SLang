package slang.utils

object AnalysePhase extends Enumeration {

  type AnalysePhase = Value
  val FileHandler, Lexer, Parser, Runtime = Value
}
