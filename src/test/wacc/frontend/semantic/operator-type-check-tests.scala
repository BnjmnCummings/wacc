package test.wacc.semantic

import wacc.semantic.*
import wacc.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

class binary_operator_type_check_test extends AnyFlatSpec {
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

class unary_operator_type_check_test extends AnyFlatSpec { 
    "not" should "accept a bool as an argument" in {
        parseAndTypeCheckStr("begin bool x = !false end") shouldBe a [Right[?, ?]]
    }

    it should "reject other types as arguments" in {
        parseAndTypeCheckStr("begin bool x = !2 end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin bool x = !\'a\' end") shouldBe a [Left[?, ?]]
    }

    it should "reject other types as output" in {
        parseAndTypeCheckStr("begin int x = !false end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin char x = !true end") shouldBe a [Left[?, ?]]
    }

    "neg" should "accept an integer as argument" in {
        parseAndTypeCheckStr("begin int x = -(3 + 3) end") shouldBe a [Right[?, ?]]
    }

    it should "reject other types as arguments" in {
        parseAndTypeCheckStr("begin int x = -true end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin int x = -\'a\' end") shouldBe a [Left[?, ?]]
    }

    it should "reject other types as output" in {
        parseAndTypeCheckStr("begin bool x = -(3 + 3) end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin string x = -(3 + 3) end") shouldBe a [Left[?, ?]]
    }

    "len" should "accept arrays as argument" in {
        parseAndTypeCheckStr("begin int[] a = [1]; int x = len a end") shouldBe a [Right[?, ?]]
        parseAndTypeCheckStr("begin char[] a = ['a']; int x = len a end") shouldBe a [Right[?, ?]]
    }

    it should "reject non array types as argument" in {
        parseAndTypeCheckStr("begin string a = \"a\"; int x = len a end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin int a = 1; int x = len a end") shouldBe a [Left[?, ?]]
    }

    it should "reject non integer types as output" in {
        parseAndTypeCheckStr("begin int[] a = [1]; char x = len a end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin int[] a = [1]; bool x = len a end") shouldBe a [Left[?, ?]]
    }

    "ord" should "accept characters as input" in {
        parseAndTypeCheckStr("begin int x = ord \'a\' end") shouldBe a [Right[?, ?]]
    }

    it should "reject other types as input" in {
        parseAndTypeCheckStr("begin int x = ord \"a\" end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin int x = ord 1 end") shouldBe a [Left[?, ?]]
    }

    it should "reject non integer types as output" in {
        parseAndTypeCheckStr("begin char x = ord \'a\' end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin int[] x = ord \'a\' end") shouldBe a [Left[?, ?]]
    }

    "chr" should "accept integers as input" in {
        parseAndTypeCheckStr("begin char x = chr 1 end") shouldBe a [Right[?, ?]]
    }

    it should "reject other types as input" in {
        parseAndTypeCheckStr("begin char x = chr 'a' end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin char x = chr true end") shouldBe a [Left[?, ?]]
    }

    it should "reject non character types as output" in {
        parseAndTypeCheckStr("begin string x = chr 1 end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin char[] x = chr 1 end") shouldBe a [Left[?, ?]]
    }
}
