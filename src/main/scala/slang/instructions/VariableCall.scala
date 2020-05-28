package slang.instructions

case class VariableCall(identifier: String,
                        maybeAttributeCall: Option[Node] = None)
    extends Node {
  override def toString(nested: Int): String = {
    val nest = getNest(nested)

    f"\n${nest}VariableCall\n${nest} identifier ${identifier}\n${nest} maybeAttributeCall: ${maybeAttributeCall
      .map(_.toString(nested + 1))
      .getOrElse("")}"
  }
}
