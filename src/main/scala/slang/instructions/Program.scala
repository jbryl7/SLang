package slang.instructions

import scala.collection.mutable.ListBuffer

case class Program(name: String) {
  def addStatement(instruction: Node) = instructions.append(instruction)
  val instructions: ListBuffer[Node] = ListBuffer()
  override def toString() =
    f"\n${name} \ninstructions\n${instructions.map(_.toString(0)).mkString}"
}
