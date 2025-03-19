package wacc

import wacc.ast.*
import wacc.error.*

import parsley.{Parsley, Result}
import parsley.quick.*
import parsley.errors.ErrorBuilder
import parsley.errors.combinator.ErrorMethods
import parsley.expr.{precedence, Ops,InfixN, InfixR, InfixL, Prefix, chain}
import lexer.{_int, _ident, _char, _string, _bool, fully}
import lexer.{BeginProg, EndProg, ThenIf, FiIf, WhileDo, WhileDone}
import lexer.implicits.implicitSymbol
import lexer.LexErrorBuilder

import scala.util.Success
import scala.util.Failure
import java.io.File
import java.io.FileNotFoundException


object parser {
    /**
      * An implicit error builder object. 
      * Collects parsing information for an accurate error message.
      */ 
    private implicit val errBuilder: ErrorBuilder[Err] = LexErrorBuilder

    /**
      * A local parser object, responsible for parsing the whole WACC program.
      * A program is simply a list of functions followed by a list of statements.
      */
    private val parser: Parsley[Prog] = fully(BeginProg ~> Prog(many(func), stmts) <~ EndProg)

    /** 
      * A parser combinator for handling function declarations.
      * Ensures they are well-formed and that they will always 'return'.
      */
    lazy val func: Parsley[Func] = atomic(
        Func(_type, 
            _ident,
            "(" ~> params <~ ")", 
            "is" ~> stmts.filter(returningBody) <~ "end"
        )
    )
    
    lazy val params: Parsley[List[Param]] = sepBy(Param(_type, _ident), ",")

    /** 
     * This helper function checks a list of statements to see if they are a returning body.
     * A 'returning body' must ultimately end in a [[wacc.ast.Return]] or a [[wacc.ast.Exit]] statement.
     * @param statements a  list of function body statements to recurse through.
     */
    private def returningBody(statements: List[Stmt]): Boolean = statements match
        case Nil => false
        case sts => sts.last match 
            case Exit(_)         => true 
            case Return(_)       => true
            case If(p, q, r)     => returningBody(q) && returningBody(r)
            case CodeBlock(sts2) => returningBody(sts2) 
            case _               => false
    
    /**
      * Parser combinators for handling program statements.
      * Statements include: function calls, declarations/assignments, loops, conditions etc.
      */
    lazy val stmts: Parsley[List[Stmt]] = sepBy1(
        (skip | decl | asgn | read | free | _return | exit | print | println | codeblock | _if | _while).label("statement"),
        ";" 
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
    lazy val codeblock: Parsley[Stmt] = CodeBlock("begin" ~> stmts <~ "end")

    lazy val _if: Parsley[Stmt] = If(
        "if" ~> expr <~ ThenIf,
        stmts,
        "else" ~> stmts <~ FiIf
    )

    lazy val _while: Parsley[Stmt] = While(
        "while" ~> expr <~ WhileDo,
        stmts <~ WhileDone
    )

    /**
      * A precedence table for handling expression parsing.
      * Atomic expressions are listed in the first argument of [[precedence()()]],
      *  and the operations are listed in the second argument in order of precedence.
      */
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
    
    /**
      * Parser combinators for handling l-values.
      * L-values include anything that belongs on the 'left hand side' of a assignment, anything that a value can be 'stored in'.
      */
    lazy val lvalue: Parsley[LValue] = pairElem | arrayElem | ident
    lazy val pairElem: Parsley[PairElem] = PairElem(("fst" as PairIndex.First) | ("snd" as PairIndex.Second), lvalue) 
    lazy val arrayElem: Parsley[ArrayElem] = atomic(ArrayElem(_ident, some("[" ~> expr <~ "]")))

    /**
      * Parser combinators for handling r-values.
      * R-values include anything that belongs on the 'right hand side' of a declaration/assignment.
      */
    lazy val rvalue: Parsley[RValue] = funcCall | expr | arrayLiteral | newPair | pairElem
    lazy val funcCall: Parsley[FuncCall] = FuncCall("call" ~> _ident, "(" ~> argList <~ ")")
    lazy val arrayLiteral: Parsley[ArrayLiteral] = ArrayLiteral("[" ~> argList <~ "]")
    lazy val newPair: Parsley[NewPair] = NewPair("newpair" ~> "(" ~> expr, "," ~> expr <~ ")")
    lazy val argList: Parsley[List[Expr]] = sepBy(expr, ",")

    /**
      * Atomic expression handlers.
      * Wrappers around the '_int', '_ident' etc. parsers which handle tokens using [[lexer.lexeme]]
      */
    lazy val int: Parsley[IntLiteral] = IntLiteral(_int)
    lazy val ident: Parsley[Ident] = Ident(_ident)
    lazy val bool: Parsley[BoolLiteral] = BoolLiteral(_bool)
    lazy val stringLiteral: Parsley[StringLiteral] = StringLiteral(_string)
    lazy val charLiteral: Parsley[CharLiteral] = CharLiteral(_char)

    /**
     * Parser combinators for handling type declarations.
     */
    lazy val _type: Parsley[Type] = arrayType | pairType | baseType 
    lazy val arrayType: Parsley[Type] = atomic(chain.postfix1(pairType | baseType)(ArrayType `from` "[]").hide)
    lazy val pairType: Parsley[Type] = PairType("pair(" ~> pairElemType <~ ",", pairElemType <~ ")")
    lazy val pairElemType: Parsley[Type] = arrayType | baseType | ("pair" as ErasedPairType)
    lazy val baseType: Parsley[Type] = 
        ("int" as BaseType.Int)  
        | ("bool"   as BaseType.Bool) 
        | ("char"   as BaseType.Char) 
        | ("string" as BaseType.String)

    /**
      * A function which parses a given file into a WACC program.
      * @param input the [[java.io.File]] to be parsed.
      * @return either an error, or an AST program.
      */
    def parseF(input: File): Result[Err, Prog] = parser.parseFile(input) match
        case Success(res) => res
        case Failure(e) => throw(FileNotFoundException())

    /**
      * A function which parses a given string into a WACC program.
      * @param input string input to be parsed.
      * @return either an error, or an AST program.
      */
    def parse(input: String): Result[Err, Prog] = parser.parse(input)
}
