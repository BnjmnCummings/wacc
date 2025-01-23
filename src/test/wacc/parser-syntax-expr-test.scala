package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import parsley.{Success, Result}

class parser_syntax_expr extends AnyFlatSpec {
    
    "parse" should "be able to parse basic expressions" in {
        parser.expr.parse("13") shouldBe Success(IntLiteral(13))
        parser.expr.parse("2 - 4") shouldBe Success(Sub(IntLiteral(2), IntLiteral(4)))
    }
}
