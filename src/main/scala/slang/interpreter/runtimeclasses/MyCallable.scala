package slang.interpreter.runtimeclasses

import slang.interpreter.{ExpressionVisitorImpl, Interpreter}

trait MyCallable {
  def call(visitor: Interpreter, args: List[Any]): Any

}
