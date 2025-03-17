package wacc

import wacc.ast.*

import parsley.{Parsley, Result}
import parsley.quick.*
import parsley.errors.ErrorBuilder
import parsley.errors.combinator.ErrorMethods
import parsley.expr.{precedence, Ops,InfixN, InfixR, InfixL, Prefix, chain}
import lexer.{_int, _ident, _char, _string, _bool, fully}
import lexer.{BeginProg, ThenIf, FiIf, WhileDo, WhileDone}
import lexer.implicits.implicitSymbol
import lexer.LexErrorBuilder

import scala.util.Success
import scala.util.Failure
import java.io.File
import java.io.FileNotFoundException


object parser {
    def parseF(input: File): Result[Err, Prog] = parser.parseFile(input) match
        case Success(res) => res
        case Failure(e) => throw(FileNotFoundException())

    def parse(input: String): Result[String, Prog] = parser.parse(input)

    private implicit val errBuilder: ErrorBuilder[Err] = LexErrorBuilder

    private val parser: Parsley[Prog] = fully(BeginProg ~> Prog(many(func), stmts) <~ "end")

    lazy val expr: Parsley[Expr] = 
        precedence(
            ("null" as PairNullLiteral),
            bool,
            int,
            charLiteral,
            stringLiteral,
            arrayElem,
            ident,
            "(" ~> expr <~ ")"
        )(
            Ops(Prefix)(
                Not `from` "!", 
                (notFollowedBy(int) ~> (Neg `from` "-")), 
                Len `from` "len", 
                Ord `from` "ord", 
                Chr `from` "chr"
            ),
            Ops(InfixL)(
                Mul `from` "*",
                Mod `from` "%", 
                Div `from` "/"
            ),
            Ops(InfixL)(
                Add `from` "+", 
                Sub `from` "-"
            ),
            Ops(InfixN)(
                GreaterThanEq `from` ">=",
                GreaterThan `from` ">",
                LessThanEq `from` "<=",
                LessThan `from` "<"
            ),
            Ops(InfixN)(
                Eq `from` "==",
                NotEq `from` "!="
            ),
            Ops(InfixR)(
                And `from` "&&"
            ),
            Ops(InfixR)(
                Or `from` "||"
            ),
        )
    

    lazy val int: Parsley[IntLiteral] = IntLiteral(_int)

    lazy val ident: Parsley[Ident] = Ident(_ident)

    lazy val bool: Parsley[BoolLiteral] = BoolLiteral(_bool)

    lazy val stringLiteral: Parsley[StringLiteral] = StringLiteral(_string)

    lazy val charLiteral: Parsley[CharLiteral] = CharLiteral(_char)

    lazy val arrayLiteral: Parsley[ArrayLiteral] = ArrayLiteral("[" ~> sepBy(expr, ",") <~ "]")

    lazy val _type: Parsley[Type] = arrayType | pairType | baseType 

    lazy val arrayType: Parsley[Type] = atomic(
        chain.postfix1(pairType | baseType)(ArrayType `from` "[]").hide
    )

    lazy val pairType: Parsley[Type] = PairType(
        "pair(" ~> pairElemType <~ ",", 
        pairElemType <~ ")"
    )

    lazy val baseType: Parsley[Type] = 
        ("int"    as BaseType.Int)  
        | ("bool"   as BaseType.Bool) 
        | ("char"   as BaseType.Char) 
        | ("string" as BaseType.String)
    

    lazy val pairElemType: Parsley[Type] = 
        arrayType 
        | baseType 
        | ("pair" as ErasedPairType)
    

    lazy val lvalue: Parsley[LValue] = pairElem | arrayElem | ident

    lazy val pairElem: Parsley[PairElem] = PairElem(
        ("fst" as PairIndex.First) | ("snd" as PairIndex.Second), 
        lvalue
    ) 

    lazy val arrayElem: Parsley[ArrayElem] = atomic(
        ArrayElem(_ident, some("[" ~> expr <~ "]"))
    )

    lazy val rvalue: Parsley[RValue] = 
        funcCall
        | expr
        | arrayLiteral
        | newPair
        | pairElem
    

    lazy val newPair: Parsley[NewPair] = NewPair(
        "newpair" ~> "(" ~> expr,
        "," ~> expr <~ ")"
    )
    

    lazy val funcCall: Parsley[FuncCall] = FuncCall(
        "call" ~> _ident, 
        "(" ~> argList <~ ")"
    )
    

    lazy val argList: Parsley[List[Expr]] = sepBy(expr, ",")

    lazy val params: Parsley[List[Param]] = sepBy(Param(_type, _ident), ",")

    lazy val func: Parsley[Func] = atomic(
        Func(_type, 
            _ident,
            "(" ~> params <~ ")", 
            "is" ~> stmts.filter(returningBody) <~ "end"
        )
    )

    lazy val skip: Parsley[Skip] =  Skip `from` "skip"

    lazy val decl: Parsley[Stmt] = Decl(_type, Ident(_ident), "=" ~> rvalue)
    
    lazy val asgn: Parsley[Stmt] = Asgn(lvalue, "=" ~> rvalue)

    lazy val read: Parsley[Stmt] = Read("read" ~> lvalue)

    lazy val free: Parsley[Stmt] = Free("free" ~> expr)

    lazy val _return: Parsley[Stmt] = Return("return" ~> expr)

    lazy val exit: Parsley[Stmt] = Exit("exit" ~> expr)

    lazy val print: Parsley[Stmt] = Print("print" ~> expr)

    lazy val println: Parsley[Stmt] = Println("println" ~> expr)
    
    lazy val _if: Parsley[Stmt] = If(
        "if" ~> expr <~ ThenIf,
        stmts,
        "else" ~> stmts <~ FiIf
    )

    lazy val _while: Parsley[Stmt] = While(
        "while" ~> expr <~ WhileDo,
        stmts <~ WhileDone
    )

    lazy val codeblock: Parsley[Stmt] = CodeBlock("begin" ~> stmts <~ "end")

    lazy val stmts: Parsley[List[Stmt]] = sepBy1(
        (skip | decl | asgn | read | free | _return | exit | print | println | codeblock | _if | _while).label("statement"),
        ";"
    )

    /** 
     * This function checks a list of statements to see if they are a returning body.
     * A 'returning body' must ultimately end in a [[wacc.ast.Return]] or a [[wacc.ast.Exit]] statement.
     * @param statements a  list of function body statements to recurse through.
     */
    def returningBody(statements: List[Stmt]): Boolean = statements match
        case Nil => false
        case sts => {
            sts.last match 
                case Exit(_)         => true 
                case Return(_)       => true
                case If(p, q, r)     => returningBody(q) && returningBody(r)
                case CodeBlock(sts2) => returningBody(sts2) 
                case _               => false
        }
}
