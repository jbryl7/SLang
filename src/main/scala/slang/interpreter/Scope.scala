package slang.interpreter

import slang.instructions.Statements._
import slang.instructions.Statements

import scala.collection.mutable

case class Scope(functions: mutable.Map[String, FunctionStatement] =
                   mutable.Map(),
                 classes: mutable.Map[String, ClassStatement] = mutable.Map(),
                 vars: mutable.Map[String, VarStatement] = mutable.Map(),
                 var parentScope: Option[Scope] = None) {

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

  def addFunction(f: FunctionStatement): Boolean = {
    val notDeclaredYet = !isInScope(f.name.lexeme)
    functions(f.name.lexeme) = f
    notDeclaredYet
  }
  def addVariable(f: VarStatement): Boolean = {
    val notDeclaredYet = !isInScope(f.name.lexeme)
    vars(f.name.lexeme) = f
    notDeclaredYet
  }
  def addClass(f: ClassStatement): Boolean = {
    val notDeclaredYet = !isInScope(f.name.lexeme)
    classes(f.name.lexeme) = f
    notDeclaredYet
  }
  def getClassDeclaration(identifier: String): Option[ClassStatement] =
    classes.get(identifier) match {
      case Some(s) => Some(s)
      case None if parentScope.isDefined =>
        parentScope.get.getClassDeclaration(identifier)
      case _ => None
    }
  def getVariableDeclaration(identifier: String): Option[VarStatement] =
    vars.get(identifier) match {
      case Some(s) => Some(s)
      case None if parentScope.isDefined =>
        parentScope.get.getVariableDeclaration(identifier)
      case _ => None
    }
  def getFunctionDeclaration(identifier: String): Option[FunctionStatement] =
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
  def toString(nested: Int): String = {
    var nest = ""
    for (x <- 0 to nested)
      nest += "  "
    f"\n${nest}Scope\n${nest} vars${vars.values.map(_.toString(nested + 1)).mkString}\n${nest} classes${classes.values
      .map(_.toString(nested + 1))
      .mkString}\n${nest} functions${functions.values.map(_.toString(nested + 1)).mkString}"
  }
}
