package test.wacc.semantic

import wacc.semantic.*
import wacc.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import wacc.semantic.Error.TypeMismatch

class operator_type_check_test extends AnyFlatSpec {
    "arithmetic operators" should "accept integers on both sides" in {
        parseAndTypeCheckStr("begin int x = 1 + 1 end") shouldBe a [Right[?, ?]]
        parseAndTypeCheckStr("begin int x = 1 * 1 end") shouldBe a [Right[?, ?]]
        parseAndTypeCheckStr("begin int x = 1 - 1 end") shouldBe a [Right[?, ?]]
        parseAndTypeCheckStr("begin int x = 1 / 1 end") shouldBe a [Right[?, ?]]
        parseAndTypeCheckStr("begin int x = 1 % 1 end") shouldBe a [Right[?, ?]]
    }

    it should "reject when either side is not an integer" in {
        parseAndTypeCheckStr("begin int x = 1 + true end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin int x = true + 1 end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin int x = 1 * 'a' end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin int x = 'a' * 1 end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin int x = 1 - true end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin int x = 1 / true end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin int x = 1 % true end") shouldBe a [Left[?, ?]]
    }

    it should "reject when output to a non integer type" in {
        parseAndTypeCheckStr("begin bool x = 1 + 1 end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin int[] x = 1 + 1 end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin bool x = 1 - 1 end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin bool x = 1 * 1 end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin bool x = 1 / 1 end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin bool x = 1 % 1 end") shouldBe a [Left[?, ?]]
    }


    "comparison operators" should "accept ints on both sides" in {
        parseAndTypeCheckStr("begin bool x = 1 >= 1 end") shouldBe a [Right[?, ?]]
        parseAndTypeCheckStr("begin bool x = 1 > 1 end") shouldBe a [Right[?, ?]]
        parseAndTypeCheckStr("begin bool x = 1 <= 1 end") shouldBe a [Right[?, ?]]
        parseAndTypeCheckStr("begin bool x = 1 < 1 end") shouldBe a [Right[?, ?]]
    }

    it should "accept chars on both sides" in {
        parseAndTypeCheckStr("begin bool x = 'a' >= 'a' end") shouldBe a [Right[?, ?]]
        parseAndTypeCheckStr("begin bool x = 'a' > 'a' end") shouldBe a [Right[?, ?]]
        parseAndTypeCheckStr("begin bool x = 'a' <= 'a' end") shouldBe a [Right[?, ?]]
        parseAndTypeCheckStr("begin bool x = 'a' < 'a' end") shouldBe a [Right[?, ?]]
    }
    
    it should "reject when either side is not an integer or char" in {
        parseAndTypeCheckStr("begin bool x = 1 >= true end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin bool x = true >= 1 end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin bool x = 1 > true end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin bool x = 'a' < true end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin bool x = 'a' <= true end") shouldBe a [Left[?, ?]]
    }

    it should "reject mismatch of chars and ints" in {
        parseAndTypeCheckStr("begin bool x = 'a' >= 1 end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin bool x = 1 > 'a' end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin bool x = 'a' <= 1 end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin bool x = 1 < 'a' end") shouldBe a [Left[?, ?]]
    }
    
    it should "reject when output to a non-boolean type" in {
        parseAndTypeCheckStr("begin int x = 1 >= 1 end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin bool[] x = 1 > 1 end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin string x = 1 <= 1 end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin char x = 1 < 1 end") shouldBe a [Left[?, ?]]
    }

    "boolean operators" should "accept bools on both sides" in {
        parseAndTypeCheckStr("begin bool x = true && false end") shouldBe a [Right[?, ?]]
        parseAndTypeCheckStr("begin bool x = true || false end") shouldBe a [Right[?, ?]]
    }

    it should "reject when either type is not a bool" in {
        parseAndTypeCheckStr("begin bool x = true && 1 end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin bool x = 'a' && false end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin bool x = true || 'a' end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin bool x = 'a' || false end") shouldBe a [Left[?, ?]]
    }

    it should "reject when output to a non-boolean type" in {
        parseAndTypeCheckStr("begin bool[] x = true && false end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin bool[] x = true || false end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin int x = true && false end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin char x = true || false end") shouldBe a [Left[?, ?]]
    }

}
