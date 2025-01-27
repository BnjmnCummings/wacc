package wacc.syntax

import wacc.parser

import wacc.lexer.fully

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import parsley.{Success, Failure, Result}

class expr_test extends AnyFlatSpec {
    "expr" should "be able to parse binary operators" in {
        parser.expr.parse("1+2") shouldBe Success(Add(IntLiteral(1),IntLiteral(2)))
    }

    it should "be able to parse unary operators" in {
        parser.expr.parse("!a") shouldBe Success(Not(Ident("a")))
    }
    
    it should "be able to parse single identifiers" in {
        parser.expr.parse("a") shouldBe Success(Ident("a"))
    }

    it should "be able to parse pair null literals" in {
        parser.expr.parse("null") shouldBe Success(PairNullLiteral)
    }

    it should "reject two variables without an operator" in {
        fully(parser.expr).parse("a b") shouldBe a [Failure[?]]
    }
}

class lvalue_test extends AnyFlatSpec {
    "lvalue" should "be able to parse single identifiers" in {
        parser.lvalue.parse("a") shouldBe Success(Ident("a"))
    }

    it should "be able to parse array elements" in {
        parser.lvalue.parse("arr[i]") shouldBe Success(ArrayElem("arr", List(Ident("i"))))
    }

    it should "be able to parse pair elements" in {
        parser.lvalue.parse("fst a") shouldBe Success(PairElem(PairIndex.First, Ident("a")))
    }

    it should "reject numbers on their own" in {
        parser.lvalue.parse("9") shouldBe a [Failure[?]]
    }

    it should "reject multiple words" in {
        fully(parser.lvalue).parse("jamie willis") shouldBe a [Failure[?]]
    }
}

class rvalue_test extends AnyFlatSpec {
    "rvalue" should "be able to parse single identifiers" in {
        parser.rvalue.parse("a") shouldBe Success(Ident("a"))
    }

    it should "be able to parse array literals" in {
        parser.rvalue.parse("[a]") shouldBe Success(ArrayLiteral(List(Ident("a"))))
    }

    it should "be able to parse newpairs" in {
        parser.rvalue.parse("newpair(a,a)") shouldBe Success(NewPair(Ident("a"), Ident("a")))
    }

    it should "be able to parse pair elements" in {
        parser.rvalue.parse("fst a") shouldBe Success(PairElem(PairIndex.First, Ident("a")))
    }

    it should "be able to parse empty function calls" in {
        parser.rvalue.parse("call a()") shouldBe Success(FuncCall("a", Nil))
    }

    it should "be able to parse function calls with arguments" in {
        parser.rvalue.parse("call a(x)") shouldBe Success(FuncCall("a", List(Ident("x"))))
    }

    it should "reject variable declarations" in {
        fully(parser.rvalue).parse("int x") shouldBe a [Failure[?]]
    }

    it should "reject pair creation without using newpair" in {
        fully(parser.rvalue).parse("pair(a,b)") shouldBe a [Failure[?]]
    }

    it should "reject function calls without the Call tag" in {
        fully(parser.rvalue).parse("foo(x)") shouldBe a [Failure[?]]
    }
}

class array_elem_test extends AnyFlatSpec {
    "arrayElem" should "be able to parse single dimensional array access" in {
        parser.arrayElem.parse("arr[a]") shouldBe Success(ArrayElem("arr", List(Ident("a"))))
    }

    it should "be able to parse multi dimensional array access" in {
        parser.arrayElem.parse("arr[a][b]") shouldBe 
            Success(ArrayElem("arr", List(Ident("a"), Ident("b"))))
    }

    it should "reject identifiers without array indices" in {
        parser.arrayElem.parse("arr") shouldBe a [Failure[?]]
    }

    it should "reject array literals" in {
        parser.arrayElem.parse("[a]") shouldBe a [Failure[?]]
    }
}

class array_literal_test extends AnyFlatSpec {
    "arrayLiteral" should "be able to parse empty arrays" in {
        parser.arrayLiteral.parse("[]") shouldBe Success(ArrayLiteral(List()))
    }

    it should "be able to parse singleton arrays" in {
        parser.arrayLiteral.parse("[a]") shouldBe Success(ArrayLiteral(List(Ident("a"))))
    }

    it should "be able to parse arrays with multiple items" in {
        parser.arrayLiteral.parse("[a, b, a]") shouldBe 
            Success(ArrayLiteral(List(Ident("a"), Ident("b"), Ident("a"))))
    }

    it should "reject missing end bracket" in {
        parser.arrayLiteral.parse("[a, b") shouldBe a [Failure[?]]
    }

    it should "reject missing comma" in {
        parser.arrayLiteral.parse("[a b]") shouldBe a [Failure[?]]
    }

    it should "reject missing brackets" in {
        parser.arrayLiteral.parse(",a") shouldBe a [Failure[?]]
    }

    it should "reject singleton variables" in {
        parser.arrayLiteral.parse("a") shouldBe a [Failure[?]]
    }
}

class atom_test extends AnyFlatSpec {
    "intLiteral" should "be able to parse positive integers" in {
        parser.int.parse("67") shouldBe Success(IntLiteral(67))
    }

    it should "be able to parse negative integers" in {
        parser.int.parse("-67") shouldBe Success(IntLiteral(-67))
    }

    it should "reject non-integer characters" in {
        parser.int.parse("a6") shouldBe a [Failure[?]]
    }

    it should "reject integers which are too big" in {
        fully(parser.int).parse("2147483648") shouldBe a [Failure[?]]
    }

    it should "reject integers which are too small" in {
        fully(parser.int).parse("-2147483649") shouldBe a [Failure[?]]
    }

    "boolLiteral" should "be able to parse booleans" in {
        parser.bool.parse("true") shouldBe Success(BoolLiteral(true))
    }

    it should "reject non boolean strings" in {
        parser.bool.parse("True") shouldBe a [Failure[?]]
    }

    "charLiteral" should "be able to parse characters" in {
        parser.charLiteral.parse("'a'") shouldBe Success(StandardCharLiteral('a'))
    }

    it should "reject empty characters" in {
        parser.charLiteral.parse("''") shouldBe a [Failure[?]]
    }

    it should "reject multiple characters" in {
        parser.charLiteral.parse("'ab'") shouldBe a [Failure[?]]
    }

    it should "accept escaped characters" in {
        parser.charLiteral.parse("'\\n'") shouldBe Success(EscCharLiteral.Newline)
    }

    "escapedChar" should "be able to parse escaped characters" in {
        parser.escapedChar.parse("\\n") shouldBe Success(EscCharLiteral.Newline)
    }

    it should "reject normal characters" in {
        parser.escapedChar.parse("a") shouldBe a [Failure[?]]
    }

    "standardChar" should "be able to parse characters" in {
        parser.standardChar.parse("n") shouldBe Success(StandardCharLiteral('n'))
    }

    it should "reject multiple characters" in {
        fully(parser.standardChar).parse("ab") shouldBe a [Failure[?]]
    }

    "ident" should "be able to parse strings" in {
        fully(parser.ident).parse("nickWu") shouldBe Success(Ident("nickWu"))
    }

    it should "be able to parse strings with numbers in them" in {
        fully(parser.ident).parse("nick2") shouldBe Success(Ident("nick2"))
    }

    it should "reject idents which begin with numbers" in {
        fully(parser.ident).parse("2nick") shouldBe a [Failure[?]]
    }

    "expr" should "be able to parse brackets" in {
        parser.expr.parse("(a)") shouldBe Success(Ident("a"))
    }

    it should "be able to parse into the correct atom" in {
        parser.expr.parse("-67") shouldBe Success(IntLiteral(-67))
        parser.expr.parse("'\\n'") shouldBe Success(EscCharLiteral.Newline)
        fully(parser.expr).parse("nickWu") shouldBe Success(Ident("nickWu"))
        fully(parser.expr).parse("nick2") shouldBe Success(Ident("nick2"))
        fully(parser.expr).parse("2nick") shouldBe a [Failure[?]]
        parser.expr.parse("true") shouldBe Success(BoolLiteral(true))
    }

    "stringLiteral" should "be able to parse empty strings" in {
        parser.stringLiteral.parse("\"\"") shouldBe Success(StringLiteral(List()))
    }

    it should "be able to parse strings with characters" in {
        parser.stringLiteral.parse("\"abc\"") shouldBe Success(
            StringLiteral(
                List(
                    StandardCharLiteral('a'), 
                    StandardCharLiteral('b'), 
                    StandardCharLiteral('c')
                )
            )
        )
    }

    it should "be able to parse strings with escaped" in {
        parser.stringLiteral.parse("\"abc\\n\"") shouldBe Success(
            StringLiteral(
                List(
                    StandardCharLiteral('a'), 
                    StandardCharLiteral('b'), 
                    StandardCharLiteral('c'),
                    EscCharLiteral.Newline
                )
            )
        )
    }
}

class binary_oper_test extends AnyFlatSpec {
    "binaryOper" should "be able to parse multiplications" in {
        parser.expr.parse("a*b") shouldBe Success(Mul(Ident("a"), Ident("b")))
    }

    it should "be able to parse divisions" in {
        parser.expr.parse("a/b") shouldBe Success(Div(Ident("a"), Ident("b")))
    }

    it should "be able to parse modulos" in {
        parser.expr.parse("a%b") shouldBe Success(Mod(Ident("a"), Ident("b")))
    }

    it should "be able to parse additions" in {
        parser.expr.parse("a+b") shouldBe Success(Add(Ident("a"), Ident("b")))
    }

    it should "be able to parse subtractions" in {
        parser.expr.parse("a-b") shouldBe Success(Sub(Ident("a"), Ident("b")))
    }

    it should "be able to parse greater thans" in {
        parser.expr.parse("a>b") shouldBe Success(GreaterThan(Ident("a"), Ident("b")))
    }

    it should "be able to parse greater than or equals" in {
        parser.expr.parse("a>=b") shouldBe Success(GreaterThanEq(Ident("a"), Ident("b")))
    }

    it should "be able to parse less thans" in {
        parser.expr.parse("a<b") shouldBe Success(LessThan(Ident("a"), Ident("b")))
    }

    it should "be able to parse less than or equals" in {
        parser.expr.parse("a<=b") shouldBe Success(LessThanEq(Ident("a"), Ident("b")))
    }

    it should "be able to parse equals" in {
        parser.expr.parse("a==b") shouldBe Success(Eq(Ident("a"), Ident("b")))
    }

    it should "be able to parse not equals" in {
        parser.expr.parse("a!=b") shouldBe Success(NotEq(Ident("a"), Ident("b")))
    }

    it should "be able to parse ands" in {
        parser.expr.parse("a&&b") shouldBe Success(And(Ident("a"), Ident("b")))
    }

    it should "be able to parse ors" in {
        parser.expr.parse("a||b") shouldBe Success(Or(Ident("a"), Ident("b")))
    }

    it should "reject two variables without an operator" in {
        parser.expr.parse("a b") shouldBe a [Failure[?]]
    }

    it should "reject illegal operators" in {
        parser.expr.parse("a ^ b") shouldBe a [Failure[?]]
    }

    it should "reject missing variable" in {
        parser.expr.parse("a +") shouldBe a [Failure[?]]
    }
}

class unary_oper_test extends AnyFlatSpec {
    "unaryOper" should "be able to parse nots" in {
        parser.unaryOper.parse("!a") shouldBe Success(Not(Ident("a")))
    }

    it should "be able to parse negations" in {
        parser.unaryOper.parse("-a") shouldBe Success(Neg(Ident("a")))
    }

    it should "be able to parse lens" in {
        parser.unaryOper.parse("len a") shouldBe Success(Len(Ident("a")))
    }

    it should "be able to parse ords" in {
        parser.unaryOper.parse("ord a") shouldBe Success(Ord(Ident("a")))
    }

    it should "be able to parse chrs" in {
        parser.unaryOper.parse("chr a") shouldBe Success(Chr(Ident("a")))
    }

    it should "reject missing operator" in {
        parser.unaryOper.parse("a") shouldBe a [Failure[?]]
    }

    it should "reject missing argument" in {
        parser.unaryOper.parse("len") shouldBe a [Failure[?]]
    }

    it should "reject invalid operator" in {
        parser.unaryOper.parse("^a") shouldBe a [Failure[?]]
    }
}

class pair_elem_test extends AnyFlatSpec {
    "pairElem" should "be able to parse first elements" in {
        parser.pairElem.parse("fst a") shouldBe Success(PairElem(PairIndex.First, Ident("a")))
    }

    it should "be able to parse second elements" in {
        parser.pairElem.parse("snd a") shouldBe Success(PairElem(PairIndex.Second, Ident("a")))
    }

    it should "reject rvalues on the right" in {
        fully(parser.pairElem).parse("snd newpair(1,2)") shouldBe a [Failure[?]]
    }

    it should "reject invalid index keyword" in {
        parser.pairElem.parse("first a") shouldBe a [Failure[?]]
    }

    it should "reject missing lvalue" in {
        parser.pairElem.parse("fst") shouldBe a [Failure[?]]
    }
}
