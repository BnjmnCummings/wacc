package wacc

import wacc.syntax.*

import parsley.{Parsley, Result}
import parsley.quick.*
import parsley.syntax.zipped.*
import parsley.errors.ErrorBuilder
import parsley.debug.*
import parsley.expr.{precedence, Ops,InfixN, InfixR, InfixL, Prefix}
import lexer.{_int, _ident, fully}
import lexer.implicits.implicitSymbol

object parser {
    def parse(input: String): Result[String, Expr] = parser.parse(input)
    private val parser = fully(expr)

    lazy val expr: Parsley[Expr] = (
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

    lazy val arrayElem: Parsley[ArrayElem] = atomic(
        (_ident,  some("[" ~> expr <~"]")) zipped (ArrayElem(_, _)) 
    ).debug("arrayElem")

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
        StringLiteral("\"" ~> many(char) <~ "\"")
    ).debug("stringLiteral")

    lazy val charLiteral: Parsley[CharLiteral] = atomic(
       "'" ~> char <~ "'"
    ).debug("standardCharLiteral")

    lazy val char = (escapedChar |standardChar)

    lazy val standardChar: Parsley[CharLiteral] = atomic(
       StandardCharLiteral(letterOrDigit)
    ).debug("standardCharLiteral")

    lazy val escapedChar: Parsley[CharLiteral] = (
        atomic("\\0"    as EscCharLiteral.Null)
        | atomic("\\b"  as EscCharLiteral.Backspace)    
        | atomic("\\n"   as EscCharLiteral.Newline)  
        | atomic("\\t"  as EscCharLiteral.Tab)                
        | atomic("\\f"  as EscCharLiteral.Formfeed)       
        | atomic("\\r"  as EscCharLiteral.CarriageReturn) 
        | atomic("\\\"" as EscCharLiteral.DoubleQuote)    
        | atomic("\\\\" as EscCharLiteral.Backslash)      
        | atomic("\\'"  as EscCharLiteral.SingleQuote) 
    ).debug("escapedCharLiteral")

    lazy val lvalue: Parsley[LValue] = ???

    lazy val rvalue: Parsley[RValue] = ???

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

    lazy val params: Parsley[List[Param]] = ???

    lazy val func: Parsley[Func] = ???

    lazy val pairElem: Parsley[PairElem] = ???

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
