package slang.instructions

case class ObjectCall(objectIdentifier: String,
                      var call: Option[Node] = None,
                      isFunction: Boolean = false)
    extends Node
    with Instruction {

  override def execute(scope: Scope): Instruction = this
}
