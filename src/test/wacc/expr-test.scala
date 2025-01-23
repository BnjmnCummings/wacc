package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import parsley.{Success, Result}

class exprTest extends AnyFlatSpec {

    // def parse(input: String): Result[String, BigInt] = parser.parse(input)
    // private val parser = fully(expr)
    // def parseExpr(input: String):Result[String, Expr] = ???
    // private val exprParser = fully(expr)

    "parse" should "be able to parse basic expressions" in {
        parser.expr.parse("13") shouldBe Success(IntLiteral(13))
        parser.expr.parse("2 - 4") shouldBe Success(Sub(IntLiteral(2), IntLiteral(4)))
    }
}
