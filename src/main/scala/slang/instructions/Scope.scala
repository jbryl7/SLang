package slang.instructions

import scala.collection.mutable

case class Scope(functions: mutable.Map[String, FunctionDeclaration] =
                   mutable.Map(),
                 classes: mutable.Map[String, ClassDeclaration] = mutable.Map(),
                 vars: mutable.Map[String, VarDeclaration] = mutable.Map()) {

  var parentScope: Scope = _
  def setParentScope(scope: Scope): Unit = parentScope = scope
  def isInScope(identifier: String): Boolean =
    isClassInScope(identifier) || isFunInScope(identifier) || isVarInScope(
      identifier)

  def isVarInScope(identifier: String): Boolean =
    vars.keys.toList.contains(identifier)
  def isFunInScope(identifier: String): Boolean =
    functions.keys.toList.contains(identifier)
  def isClassInScope(identifier: String): Boolean =
    classes.keys.toList.contains(identifier)

  def addFunction(f: FunctionDeclaration): Boolean = {
    val notDeclaredYet = !isInScope(f.identifier)
    functions(f.identifier) = f
    notDeclaredYet
  }
  def addVariable(f: VarDeclaration): Boolean = {
    val notDeclaredYet = !isInScope(f.getIdentifier)
    vars(f.getIdentifier) = f
    notDeclaredYet
  }
  def addClass(f: ClassDeclaration): Boolean = {
    val notDeclaredYet = !isInScope(f.identifier)
    classes(f.identifier) = f
    notDeclaredYet
  }

  def getFunctionsIdentifiers() = functions.keys
  def getClassIdentifiers() = classes.keys
  def getVariableIdentifiers() = vars.keys
  def getClasses() = classes.keys
}
