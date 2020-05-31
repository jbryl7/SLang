package slang.interpreter.runtimeclasses

import slang.instructions.statements.FunctionStatement
import slang.interpreter.Scope

case class MyFunction(declaration: FunctionStatement, scope: Scope)
    extends MyCallable
