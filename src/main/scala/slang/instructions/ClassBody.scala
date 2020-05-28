package slang.instructions

import scala.collection.mutable.ListBuffer

case class ClassBody(vars: ListBuffer[VarDeclaration] = ListBuffer(),
                     funs: ListBuffer[FunctionDeclaration] = ListBuffer())
    extends Node {

  override def toString(nested: Int): String = {
    val nest = getNest(nested)

    f"\n${nest} classBody \n${nest} vars:${vars
      .map(_.toString(nested + 1))
      .mkString("")}\n ${nest}functions: ${funs.map(_.toString(nested + 1)).mkString("")}"
  }
}
