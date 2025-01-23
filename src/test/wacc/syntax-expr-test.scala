package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import parsley.{Success, Failure, Result}

class syntax_expr extends AnyFlatSpec {    
    "expr" should "be able to parse binary operators" in {
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
