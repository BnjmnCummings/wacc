package wacc.syntax

import wacc.parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import parsley.{Success, Failure, Result}

class expr_test extends AnyFlatSpec {
    "expr" should "be able to parse binary operators" in {
        parser.expr.parse("1 + 2") shouldBe Success(Add(IntLiteral(1),IntLiteral(2)))
    }
 
    it should "be able to parse binary operators" in {
        parser.expr.parse("a + b") shouldBe Success(Add(Ident("a"),Ident("b")))
    }

    it should "be able to parse unary operators" in {
        parser.expr.parse("!a") shouldBe Success(Not(Ident("a")))
    }
    
    it should "be able to parse single identifiers" in {
        parser.expr.parse("a") shouldBe Success(Ident("a"))
    }

    it should "reject two variables without an operator" in {
        parser.expr.parse("a b") shouldBe a [Failure[?]]
    }
}

class lvalue extends AnyFlatSpec {
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
        parser.lvalue.parse("jamie willis") shouldBe a [Failure[?]]
    }
}

class rvalue extends AnyFlatSpec {
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
        parser.rvalue.parse("int x") shouldBe a [Failure[?]]
    }

    it should "reject pair creation without using newpair" in {
        parser.rvalue.parse("pair(a,b)") shouldBe a [Failure[?]]
    }

    it should "reject function calls without the Call tag" in {
        parser.rvalue.parse("foo(x)") shouldBe a [Failure[?]]
    }
}

class array_elem extends AnyFlatSpec {
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

class array_literal extends AnyFlatSpec {
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

class atom extends AnyFlatSpec {
    // case class IntLiteral(v: BigInt) extends Expr
    // case class BoolLiteral(v: Boolean) extends Expr
    // sealed abstract class CharLiteral extends Expr
    // case class EscapedCharLiteral(v: Char) extends CharLiteral
    // case class StandardCharLiteral(v: Char) extends CharLiteral
    // case class StringLiteral(v: List[CharLiteral]) extends Expr
    // case class Ident(v: String) extends Expr, LValue
    // case class ArrayElem(v: String, is: List[Expr]) extends Expr, LValue

    // object PairNullLiteral extends Expr
    "intiteral" should "be able to parse empty arrays" in {
        parser.arrayLiteral.parse("[]") shouldBe Success(ArrayLiteral(List()))
    }

}

class binary_oper extends AnyFlatSpec {

}

class pair_elem extends AnyFlatSpec {

}

class unary_oper extends AnyFlatSpec {

}