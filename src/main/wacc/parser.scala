package wacc

import wacc.syntax.*

import parsley.{Parsley, Result}
import parsley.quick.*
import parsley.syntax.zipped.*
import parsley.errors.ErrorBuilder
import parsley.debug.*
import parsley.expr.{precedence, Ops,InfixN, InfixR, InfixL, Prefix}
import lexer.{_int, _ident, _char, _string, fully}
import lexer.implicits.implicitSymbol

object parser {
    def parse(input: String): Result[String, Expr] = parser.parse(input)
    private val parser = fully(expr)

    lazy val expr: Parsley[Expr] = atomic(
        precedence(
            atomic("null" as PairNullLiteral),
            bool,
            int,
            charLiteral,
            stringLiteral,
            arrayElem,
            ident,
            "(" ~> expr <~ ")"
        )(
            Ops(Prefix)(
                Not from "!", 
                Neg from "-", 
                Len from "len", 
                Ord from "ord", 
                Chr from "chr"
            ),
            Ops(InfixL)(
                Mul from "*",
                Mod from "%", 
                Div from "/"
            ),
            Ops(InfixL)(
                Add from "+", 
                Sub from "-"
            ),
            Ops(InfixN)(
                GreaterThanEq from ">=",
                GreaterThan from ">",
                LessThanEq from "<=",
                LessThan from "<"
            ),
            Ops(InfixN)(
                Eq from "==",
                NotEq from "!="
            ),
            Ops(InfixR)(
                And from "&&"
            ),
            Ops(InfixR)(
                Or from "||"
            ),
        )
    ).debug("expr")

    lazy val int: Parsley[IntLiteral] = atomic(
        IntLiteral(_int)
    ).debug("int")

    lazy val ident: Parsley[Ident] = atomic(
        Ident(_ident)
    ).debug("identity")

    lazy val bool: Parsley[BoolLiteral] = atomic(
        BoolLiteral(("true" as true) | ("false" as false))
    ).debug("boolLiteral")

    lazy val stringLiteral: Parsley[StringLiteral] = atomic(
        StringLiteral(_string)
    ).debug("stringLiteral")

    lazy val charLiteral: Parsley[CharLiteral] = atomic(
       CharLiteral(_char)
    ).debug("charLiteral")

    lazy val arrayLiteral: Parsley[ArrayLiteral] = (
        ArrayLiteral("[" ~> sepBy(expr, ",") <~ "]")
    ).debug("arrayLiteral")

    lazy val _type: Parsley[Type] = atomic(
        arrayType 
        | pairType  
        | baseType 
    ).debug("type")

    lazy val arrayType: Parsley[Type] = atomic(
        ((pairType | baseType),  some("[]")) zipped (
            (t, bs) => bs.foldLeft(t)((acc, _) => ArrayType(acc))
        )    
    ).debug("arrayType")

    lazy val pairType: Parsley[Type] = atomic(
        PairType(
            "pair(" ~> pairElemType <~ ",", 
            pairElemType <~ ")"
        )
    ).debug("pairType")

    lazy val baseType: Parsley[Type] = (
        ("int"    as BaseType.Int)  
        | ("bool"   as BaseType.Bool) 
        | ("char"   as BaseType.Char) 
        | ("string" as BaseType.String)
    ).debug("baseType")

    lazy val pairElemType: Parsley[Type] = atomic(
        arrayType 
        | baseType  
        | ("pair" as ErasedPairType)
    ).debug("pairElemType")

    lazy val lvalue: Parsley[LValue] = pairElem | arrayElem | ident

    lazy val pairElem: Parsley[PairElem] = atomic(
        PairElem(
            ("fst" as PairIndex.First) | ("snd" as PairIndex.Second), 
            lvalue
        )  
    ).debug("pairElem")

    lazy val arrayElem: Parsley[ArrayElem] = atomic(
        (_ident,  some("[" ~> expr <~"]")) zipped (ArrayElem(_, _)) 
    ).debug("arrayElem")

    lazy val rvalue: Parsley[RValue] = (
        funcCall
        | expr
        | arrayLiteral
        | newPair
        | pairElem
    ).debug("rvalue")

    lazy val newPair: Parsley[NewPair] = atomic(
        NewPair(
            "newpair" ~> "(" ~> expr,
            "," ~> expr <~ ")"
        )
    ).debug("newPair")

    lazy val funcCall: Parsley[FuncCall] = atomic(
        FuncCall(
            "call" ~> _ident, 
            "(" ~> argList <~ ")"
        )
    ).debug("funcCall")

    lazy val argList: Parsley[List[Expr]] = (
        sepBy(expr, ",")
    ).debug("argList")

    lazy val params: Parsley[List[Param]] = ???

    lazy val func: Parsley[Func] = ???

    lazy val decl: Parsley[Stmt] = ???
    
    lazy val asgn: Parsley[Stmt] = ???

    lazy val read: Parsley[Stmt] = ???

    lazy val free: Parsley[Stmt] = ???

    lazy val _return: Parsley[Stmt] = ???

    lazy val exit: Parsley[Stmt] = ???

    lazy val print: Parsley[Stmt] = ???

    lazy val println: Parsley[Stmt] = ???
    
    lazy val _if: Parsley[Stmt] = ???

    lazy val _while: Parsley[Stmt] = ???

    lazy val codeblock: Parsley[Stmt] = ???

    lazy val stmts: Parsley[List[Stmt]] = ???
}
