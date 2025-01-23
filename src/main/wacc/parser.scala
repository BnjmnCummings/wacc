package wacc

import parsley.{Parsley, Result}
// import parsley.expr.chain
// import parsley.debug.*

import lexer.implicits.implicitSymbol
import lexer.{integer, fully}
import parsley.expr.precedence
import parsley.expr.{Ops, InfixL}

object parser {
    def parse(input: String): Result[String, Expr] = parser.parse(input)
    private val parser = fully(expr)

    protected[wacc] lazy val expr: Parsley[Expr] =
        precedence(integer.map(IntLiteral(_)), "(" ~> expr <~ ")")(
            Ops(InfixL)("+" as (Add(_,_))),
            Ops(InfixL)("-" as (Sub(_,_)))
        )
}
