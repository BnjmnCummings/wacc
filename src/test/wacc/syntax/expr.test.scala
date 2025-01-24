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
    
    it should "be able to parse identifiers on their own" in {
        parser.expr.parse("a") shouldBe Success(Ident("a"))
    }

    it should "reject two variables without an operator" in {
        parser.expr.parse("a b") shouldBe a [Failure[?]]
    }
}

class lvalue extends AnyFlatSpec {

}

class rvalue extends AnyFlatSpec {

}

class array_elem extends AnyFlatSpec {

}

class array_literal extends AnyFlatSpec {

}

class atom extends AnyFlatSpec {

}

class binary_oper extends AnyFlatSpec {

}

class pair_elem extends AnyFlatSpec {

}

class unary_oper extends AnyFlatSpec {

}