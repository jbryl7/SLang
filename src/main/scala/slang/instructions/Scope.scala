package slang.instructions

import scala.collection.mutable.ListBuffer

case class Scope() {

  val functions: collection.mutable.Map[String, FunctionDeclaration] =
    collection.mutable.Map()
  var parentScope: Scope = null
  def setParentScope(scope: Scope) = parentScope = scope
  def isInScope(identifier: String) =
    functions.keys.toList.contains(identifier) // todo add vals
  def addFunction(f: FunctionDeclaration) = {
    val notDeclaredYet = !isInScope(f.identifier)
    functions(f.identifier) = f
    notDeclaredYet
  }
  def getIdentifiers() = functions.keys
  def getClasses() = functions.keys // todo add classes
}
