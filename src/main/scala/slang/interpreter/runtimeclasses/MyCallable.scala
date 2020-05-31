package slang.interpreter.runtimeclasses

import slang.interpreter.Interpreter

import scala.collection.mutable.ListBuffer

trait MyCallable {
  def call(visitor: Interpreter, args: ListBuffer[Any]): Any

}
