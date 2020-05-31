package slang.interpreter

import slang.instructions.expressions.Expression
import slang.instructions.statements._

trait StatementVisitorImpl
    extends StatementVisitor[Unit]
    with ExpressionVisitorImpl {}
