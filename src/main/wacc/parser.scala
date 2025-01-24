package wacc

import parsley.{Parsley, Result}
// import parsley.expr.chain
// import parsley.debug.*

import wacc.syntax.*

import lexer.implicits.implicitSymbol
import lexer.{integer, fully}
import parsley.expr.precedence
import parsley.expr.{Ops, InfixL}

object parser {
    def parse(input: String): Result[String, Expr] = parser.parse(input)
    private val parser = fully(expr)

    lazy val expr: Parsley[Expr] =
        precedence(integer.map(IntLiteral(_)), "(" ~> expr <~ ")")(
            Ops(InfixL)("+" as (Add(_,_))),
            Ops(InfixL)("-" as (Sub(_,_)))
        )

    lazy val lvalue: Parsley[LValue] = ???

    lazy val rvalue: Parsley[RValue] = ???

    lazy val arrayElem: Parsley[ArrayElem] = ???

    lazy val arrayLiteral: Parsley[ArrayLiteral] = ???

    lazy val intLiteral: Parsley[IntLiteral] = ???

    lazy val boolLiteral: Parsley[BoolLiteral] = ???

    lazy val escapedCharLiteral: Parsley[EscapedCharLiteral] = ???

    lazy val standardCharLiteral: Parsley[StandardCharLiteral] = ???

    lazy val stringLiteral: Parsley[StringLiteral] = ???

    lazy val ident: Parsley[Ident] = ???

    lazy val binaryOper: Parsley[BinaryOper] = ???

    lazy val unaryOper: Parsley[UnaryOper] = ???
}
