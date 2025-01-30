package wacc

import wacc.syntax.*

import parsley.{Parsley, Result}
import parsley.quick.*
import parsley.syntax.zipped.*
import parsley.errors.ErrorBuilder
import parsley.debug.*
import parsley.expr.{precedence, Ops,InfixN, InfixR, InfixL, Prefix}
import lexer.{_int, _ident, _char, _string, _bool, fully}
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
    )//.debug("expr")

    lazy val int: Parsley[IntLiteral] = atomic(
        IntLiteral(_int)
    )//.debug("int")

    lazy val ident: Parsley[Ident] = atomic(
        Ident(_ident)
    )//.debug("identity")

    lazy val bool: Parsley[BoolLiteral] = atomic(
        BoolLiteral(_bool)
    )//.debug("boolLiteral")

    lazy val stringLiteral: Parsley[StringLiteral] = atomic(
        StringLiteral(_string)
    )//.debug("stringLiteral")

    lazy val charLiteral: Parsley[CharLiteral] = atomic(
       CharLiteral(_char)
    )//.debug("charLiteral")

    lazy val arrayLiteral: Parsley[ArrayLiteral] = (
        ArrayLiteral("[" ~> sepBy(expr, ",") <~ "]")
    )//.debug("arrayLiteral")

    lazy val _type: Parsley[Type] = atomic(
        arrayType 
        | pairType  
        | baseType 
    )//.debug("type")

    lazy val arrayType: Parsley[Type] = atomic(
        ((pairType | baseType),  some("[]")) zipped (
            (t, bs) => bs.foldLeft(t)((acc, _) => ArrayType(acc))
        )    
    )//.debug("arrayType")

    lazy val pairType: Parsley[Type] = atomic(
        PairType(
            "pair(" ~> pairElemType <~ ",", 
            pairElemType <~ ")"
        )
    )//.debug("pairType")

    lazy val baseType: Parsley[Type] = (
        ("int"    as BaseType.Int)  
        | ("bool"   as BaseType.Bool) 
        | ("char"   as BaseType.Char) 
        | ("string" as BaseType.String)
    )//.debug("baseType")

    lazy val pairElemType: Parsley[Type] = atomic(
        arrayType 
        | baseType  
        | ("pair" as ErasedPairType)
    )//.debug("pairElemType")

    lazy val lvalue: Parsley[LValue] = pairElem | arrayElem | ident

    lazy val pairElem: Parsley[PairElem] = atomic(
        PairElem(
            ("fst" as PairIndex.First) | ("snd" as PairIndex.Second), 
            lvalue
        )  
    )//.debug("pairElem")

    lazy val arrayElem: Parsley[ArrayElem] = atomic(
        (_ident,  some("[" ~> expr <~"]")) zipped (ArrayElem(_, _)) 
    )//.debug("arrayElem")

    lazy val rvalue: Parsley[RValue] = (
        funcCall
        | expr
        | arrayLiteral
        | newPair
        | pairElem
    )//.debug("rvalue")

    lazy val newPair: Parsley[NewPair] = atomic(
        NewPair(
            "newpair" ~> "(" ~> expr,
            "," ~> expr <~ ")"
        )
    )//.debug("newPair")

    lazy val funcCall: Parsley[FuncCall] = atomic(
        FuncCall(
            "call" ~> _ident, 
            "(" ~> argList <~ ")"
        )
    )//.debug("funcCall")

    lazy val argList: Parsley[List[Expr]] = (
        sepBy(expr, ",")
    )//.debug("argList")

    lazy val params: Parsley[List[Param]] = atomic(
        sepBy(
            Param(_type, _ident),
            ","
        )
    )//.debug("params")

    lazy val func: Parsley[Func] = 
        Func(_type, 
            _ident,
            "(" ~> params <~ ")", 
            "is" ~> stmts.map(Some(_)).mapFilter(returningBody) <~ "end"
        ).debug("func")

    lazy val skip: Parsley[Skip.type] = atomic(
        "skip" as Skip
    )//.debug("skip")

    lazy val decl: Parsley[Stmt] = atomic(
        Decl(_type, _ident, "=" ~> rvalue)
    )//.debug("decl")
    
    lazy val asgn: Parsley[Stmt] = atomic(
        Asgn(lvalue, "=" ~> rvalue)
    )//.debug("asgn")

    lazy val read: Parsley[Stmt] = atomic(
        Read("read" ~> lvalue)
    )//.debug("read")

    lazy val free: Parsley[Stmt] = atomic(
        Free("free" ~> expr)
    )//.debug("free")

    lazy val _return: Parsley[Stmt] = atomic(
        Return("return" ~> expr)
    )//.debug("return")

    lazy val exit: Parsley[Stmt] = atomic(
        Exit("exit" ~> expr)
    )//.debug("exit")

    lazy val print: Parsley[Stmt] = atomic(
        Print("print" ~> expr)
    )//.debug("print")

    lazy val println: Parsley[Stmt] = atomic(
        Println("println" ~> expr)
    )//.debug("println")
    
    lazy val _if: Parsley[Stmt] = atomic(
        If(
            "if" ~> expr <~ "then",
            stmts,
            "else" ~> stmts <~ "fi"
        )
    )//.debug("if")

    lazy val _while: Parsley[Stmt] = atomic(
        While(
            "while" ~> expr <~ "do",
            stmts <~ "done"
        )
    )//.debug("while")

    lazy val codeblock: Parsley[Stmt] = atomic(
        CodeBlock("begin" ~> stmts <~ "end")
    )//.debug("codeblock")

    lazy val stmts: Parsley[List[Stmt]] = atomic(
        sepBy1(
            (skip | decl | asgn | read | free | _return | exit | print | println | codeblock | _if | _while),
            ";"
        )
    )//.debug("stmts")

    /* 
    This function checks a list of statements wrapped in an Option to see if they are a returning body
    It returns the result as an Some if they are and a None if not
    */
    def returningBody(st_opt: Option[List[Stmt]]): Option[List[Stmt]] = st_opt match
        case None => None
        case Some(sts) => {
            sts.last match 
                // if it ends with exit or return we're good
                case Exit(_) => Some(sts) 
                case Return(_) => Some(sts)
                // recursive call if the last statement is an if
                case If(p, q, r) => {
                    val qs: Option[List[Stmt]] = returningBody(Some(q)) 
                    val rs: Option[List[Stmt]] = returningBody(Some(r))
                    (qs, rs) match 
                        case (Some(_), Some(_)) => Some(sts)
                        case _ => None
                }
                // if it ends with anything else its not a returning body
                case _ => None
        }
}
