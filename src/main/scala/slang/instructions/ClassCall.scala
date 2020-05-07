package slang.instructions

case class ObjectCall(ObjectIdentifier: String) extends Node with Instruction {
  var call: Option[Node] = None
  val isFunction: Boolean = false

  override def execute(scope: Scope): Instruction = this
}
