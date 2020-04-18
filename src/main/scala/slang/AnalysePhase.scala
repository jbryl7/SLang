package slang

object AnalysePhase extends Enumeration {
  type AnalysePhase = Value
  val FileHandler, Lexer, Parser = Value
}
