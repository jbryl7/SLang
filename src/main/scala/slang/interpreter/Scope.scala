package slang.interpreter

import slang.instructions.statements._

import scala.collection.mutable

case class Scope(functions: mutable.Map[String, FunctionStatement] =
                   mutable.Map(),
                 classes: mutable.Map[String, ClassStatement] = mutable.Map(),
                 vars: mutable.Map[String, VarStatement] = mutable.Map(),
                 var parentScope: Scope = null) {

//  def isInScope(identifier: String): Boolean =
//    isClassInScope(identifier) || isFunInScope(identifier) || isVarInScope(
//      identifier)
//
//  def isVarInScope(identifier: String): Boolean =
//    vars.keys.toList.contains(identifier) || parentScope.isVarInScope(identifier)
//  def isFunInScope(identifier: String): Boolean =
//    functions.keys.toList.contains(identifier) || parentScope.exists(
//      _.isFunInScope(identifier))
//  def isClassInScope(identifier: String): Boolean =
//    classes.keys.toList.contains(identifier) || parentScope.exists(
//      _.isClassInScope(identifier))
//
//  def addFunction(f: FunctionStatement): Boolean = {
//    val notDeclaredYet = !isInScope(f.name.lexeme)
//    functions(f.name.lexeme) = f
//    notDeclaredYet
//  }
//  def addVariable(f: VarStatement): Boolean = {
//    val notDeclaredYet = !isInScope(f.name.lexeme)
//    vars(f.name.lexeme) = f
//    notDeclaredYet
//  }
//  def addClass(f: ClassStatement): Boolean = {
//    val notDeclaredYet = !isInScope(f.name.lexeme)
//    classes(f.name.lexeme) = f
//    notDeclaredYet
//  }
//  def getClassDeclaration(identifier: String): ClassStatement =
//    classes(identifier) match {
//      case s => s
//      case _ if parentScope != null =>
//        parentScope.get.getClassDeclaration(identifier)
//      case _ => null
//    }
//  def getVariableDeclaration(identifier: String): VarStatement =
//    vars.get(identifier) match {
//      case s if s != null => s
//      case _ if parentScope != null =>
//        parentScope.getFunctionDeclaration(identifier)
//      case _ => null
//    }
//  def getFunctionDeclaration(identifier: String): FunctionStatement =
//    functions(identifier) match {
//      case s if s != null => s
//      case _ if parentScope != null =>
//        parentScope.get.getFunctionDeclaration(identifier)
//      case _ => null
//    }
//  def getFunctionsIdentifiers: List[String] =
//    functions.keys.toList ++ parentScope.toList
//      .flatMap(_.getFunctionsIdentifiers)
//      .toList
//  def getVariableIdentifiers: List[String] =
//    vars.keys.toList ++ parentScope.toList
//      .flatMap(_.getVariableIdentifiers)
//      .toList
//  def getClassesIdentifiers: List[String] =
//    classes.keys.toList ++ parentScope.toList
//      .flatMap(_.getClassesIdentifiers)
//      .toList
  def toString(nested: Int): String = {
    var nest = ""
    for (x <- 0 to nested)
      nest += "  "
    f"\n${nest}Scope\n${nest} vars${vars.values.map(_.toString(nested + 1)).mkString}\n${nest} classes${classes.values
      .map(_.toString(nested + 1))
      .mkString}\n${nest} functions${functions.values.map(_.toString(nested + 1)).mkString}"
  }
}
