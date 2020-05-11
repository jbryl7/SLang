package slang.instructions

case class ObjectCall(objectIdentifier: String,
                      var call: Option[Node] = None,
                      isFunction: Boolean = false)
    extends Node
    with Instruction {

  override def execute(scope: Scope): Instruction = this

  override def toString(nested: Int): String = {
    val nest = getNest(nested)
    f"\n${nest}ObjectCall\n${nest} objectIdentifier ${objectIdentifier}\n${nest} isFun: ${isFunction}\n${nest} Call:${call
      .map(_.toString(nested + 1))
      .getOrElse("")}"
  }
}
