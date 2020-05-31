package slang.interpreter

import slang.instructions.expressions._
import slang.interpreter.runtimeclasses.{MyCallable, MyInstance}
import slang.lexer.{Token, TokenType}
import slang.utils._

trait ExpressionVisitorImpl extends ExpressionVisitor[Any] {}
