package slang.instructions

case class Return(returnStatement: Node) extends Node {

  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}return\n${nest} ${returnStatement}"
  }
}
