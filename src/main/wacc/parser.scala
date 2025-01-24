package wacc

import parsley.{Parsley, Result}
// import parsley.expr.chain
// import parsley.debug.*

import wacc.syntax.*

import lexer.implicits.implicitSymbol
import lexer.{integer, fully}
import parsley.expr.precedence
import parsley.quick.*
import parsley.syntax.zipped.*
import parsley.expr.{precedence, Ops, InfixL, chain}
import parsley.errors.ErrorBuilder
import parsley.debug.*
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

    lazy val _type: Parsley[Type] =  ??? // arrayType | pairType | baseType 

    lazy val arrayType: Parsley[Type] = ??? // atomic(_type <~ "[]") 

    lazy val pairType: Parsley[Type] = ??? // atomic(("pair(" ~> pairElemType, "," ~> pairElemType <~ ")").zipped(PairType(_, _))).debug("pear")

    lazy val baseType: Parsley[Type] = ??? // ("int" as BaseType.Int) |  ("bool" as BaseType.Bool) |  ("char" as BaseType.Char) | ("string" as BaseType.String)

    lazy val pairElemType: Parsley[Type] = ??? //"pair".as(ErasedPairType) | arrayType | baseType
}
