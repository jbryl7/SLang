package slang.instructions

import slang.utils._
import scala.collection.mutable

case class Scope(functions: mutable.Map[String, FunctionDeclaration] =
                   mutable.Map(),
                 classes: mutable.Map[String, ClassDeclaration] = mutable.Map(),
                 vars: mutable.Map[String, VarDeclaration] = mutable.Map(),
                 var parentScope: Option[Scope] = None) {

  def setParentScope(scope: Option[Scope]): Unit = parentScope = scope
  def isInScope(identifier: String): Boolean =
    isClassInScope(identifier) || isFunInScope(identifier) || isVarInScope(
      identifier)

  def isVarInScope(identifier: String): Boolean =
    vars.keys.toList.contains(identifier) || parentScope.exists(
      _.isVarInScope(identifier))
  def isFunInScope(identifier: String): Boolean =
    functions.keys.toList.contains(identifier) || parentScope.exists(
      _.isFunInScope(identifier))
  def isClassInScope(identifier: String): Boolean =
    classes.keys.toList.contains(identifier) || parentScope.exists(
      _.isClassInScope(identifier))

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
  def getClassDeclaration(identifier: String): Option[ClassDeclaration] =
    classes.get(identifier) match {
      case Some(s) => Some(s)
      case None if parentScope.isDefined =>
        parentScope.get.getClassDeclaration(identifier)
      case _ => None
    }
  def getVariableDeclaration(identifier: String): Option[VarDeclaration] =
    vars.get(identifier) match {
      case Some(s) => Some(s)
      case None if parentScope.isDefined =>
        parentScope.get.getVariableDeclaration(identifier)
      case _ => None
    }
  def getFunctionDeclaration(identifier: String): Option[FunctionDeclaration] =
    functions.get(identifier) match {
      case Some(s) => Some(s)
      case None if parentScope.isDefined =>
        parentScope.get.getFunctionDeclaration(identifier)
      case _ => None
    }
  def getFunctionsIdentifiers: List[String] =
    functions.keys.toList ++ parentScope.toList
      .flatMap(_.getFunctionsIdentifiers)
      .toList
  def getVariableIdentifiers: List[String] =
    vars.keys.toList ++ parentScope.toList
      .flatMap(_.getVariableIdentifiers)
      .toList
  def getClassesIdentifiers: List[String] =
    classes.keys.toList ++ parentScope.toList
      .flatMap(_.getClassesIdentifiers)
      .toList

}
