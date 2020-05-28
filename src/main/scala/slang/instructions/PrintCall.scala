package slang.instructions

case class PrintCall(expression: Node) extends Node {
  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}PrintCall\n${nest} printable: ${expression.toString(nested + 1)}"
  }
}
