package test.wacc.semantic

import wacc.semantic.*
import wacc.*
import wacc.q_ast.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import wacc.semantic.Error.TypeMismatch

class types_test extends AnyFlatSpec {
    "free" should "be able to free arrays" in {
        parseAndTypeCheckStr("begin int[] a = []; free a end") shouldBe a [Right[?, ?]]
    }

    it should "be able to free pairs" in {
        parseAndTypeCheckStr("begin pair(int,int) a = null; free a end") shouldBe a [Right[?, ?]]
    }

    it should "reject strings" in {
        parseAndTypeCheckStr("begin string s = \"adam marshall\"; free s end") shouldBe a [Left[?, ?]]
    }

    "type checker" should "allow char[] to take the place of string" in {
        parseAndTypeCheckStr("begin char[] s = [\'a\']; string s2 = s end") shouldBe a [Right[?, ?]]
    }

    it should "not allow string to take the place of char[]" in {
        parseAndTypeCheckStr("begin string s = \"a\"; char[] s2 = s end") shouldBe a [Left[?, ?]]
    }

    it should "not allow string[] to take the place of char[][]" in {
        parseAndTypeCheckStr("begin char[] s0 = [\'a\']; char[][] s = [s0]; string[] s2 = s end") shouldBe a [Left[?, ?]]
    }

    it should "allow a char[] to be put in a string[]" in {
        parseAndTypeCheckStr("begin char[] s0 = [\'a\']; string[] s2 = [s0] end") shouldBe a [Right[?, ?]]
    }

    it should "allow nested pairs" in {
        parseAndTypeCheckStr("begin pair(int, int) p = newpair(1, 2); pair(pair, pair) nestedPair = newpair(p, p) end") shouldBe a [Right[?, ?]]
    }

    "return" should "not be allowed outside function bodies" in {
        parseAndTypeCheckStr("begin return 7 end") shouldBe a [Left[?, ?]]
    }

    it should "type check successfully with the correct type inside a function body" in {
        parseAndTypeCheckStr("begin int x() is return 7 end skip end") shouldBe a [Right[?, ?]]
    }

    it should "fail when the incorrect type is returned" in {
        parseAndTypeCheckStr("begin int x() is return \'a\' end skip end") shouldBe a [Left[?, ?]]
    }

    "read" should "accept an integer value" in {
        parseAndTypeCheckStr("begin int x = 0; read x end") shouldBe a [Right[?, ?]]
    }

    it should "accept a char value" in {
        parseAndTypeCheckStr("begin char a = 'a'; read a end") shouldBe a [Right[?, ?]]
    }

    it should "reject any other types" in {
        parseAndTypeCheckStr("begin string a = \"a\"; read a end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin bool a = true; read a end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin int[] a = [9]; read a end") shouldBe a [Left[?, ?]]
    }

    "exit" should "accept an integer value" in {
        parseAndTypeCheckStr("begin exit 0 end") shouldBe a [Right[?, ?]]
        parseAndTypeCheckStr("begin exit 3 + 4 end") shouldBe a [Right[?, ?]]
    }
    
    it should "reject any other type" in {
        parseAndTypeCheckStr("begin exit \'a\' end") shouldBe a [Left[?, ?]]
        parseAndTypeCheckStr("begin exit true end") shouldBe a [Left[?, ?]]
    }

    "if statement" should "accept a boolean condition" in {
        parseAndTypeCheckStr("begin if (3 == 3) then skip else skip fi end") shouldBe a [Right[?, ?]]
    }

    it should "reject any other type of condition" in {
        parseAndTypeCheckStr("begin if (3 + 3) then skip else skip fi end") shouldBe a [Left[?, ?]]
    }

    "while loop" should "accept a boolean condition" in {
        parseAndTypeCheckStr("begin while (3 == 3) do skip done end") shouldBe a [Right[?, ?]]
    }

    it should "reject any other type of condition" in {
        parseAndTypeCheckStr("begin while (3 + 3) do skip done end") shouldBe a [Left[?, ?]]
    }

    "declaration" should "pass type checks when both sides are int type" in {
        parseAndTypeCheckStr("begin int x = 7 end") shouldBe a [Right[?, ?]]
        parseAndTypeCheckStr("begin int x = 7 + 7 end") shouldBe a [Right[?, ?]]
    }

    it should "pass type checks when both sides are array type" in {
        parseAndTypeCheckStr("begin int[] x = [1, 2] end") shouldBe a [Right[?, ?]]
        parseAndTypeCheckStr("begin int[] x = [1, 2 + 3] end") shouldBe a [Right[?, ?]]
    }

    it should "fail type checks when both sides have different types" in {
        parseAndTypeCheckStr("begin int x = \'a\' end") shouldBe a [Left[?, ?]]
    }

    it should "fail type checks when both sides have different array types" in {
        parseAndTypeCheckStr("begin int[] x = [\'a\'] end") shouldBe a [Left[?, ?]]
    }

    it should "fail type checks when RHS is an array with multiple types in it" in {
        parseAndTypeCheckStr("begin int[] x = [9, true] end") shouldBe a [Left[?, ?]]
    }

    "assignment" should "pass type checks when both sides are int type" in {
        parseAndTypeCheckStr("begin int x = 7; x = 0 end") shouldBe a [Right[?, ?]]
        parseAndTypeCheckStr("begin int x = 7 + 7; x = 0 end") shouldBe a [Right[?, ?]]
    }

    it should "pass type checks when both sides are array type" in {
        parseAndTypeCheckStr("begin int[] x = [1, 2]; x = [1] end") shouldBe a [Right[?, ?]]
        parseAndTypeCheckStr("begin int[] x = [1, 2]; x = [1 + 1] end") shouldBe a [Right[?, ?]]
    }

    it should "fail type checks when both sides have different types" in {
        parseAndTypeCheckStr("begin int x = 3; x = 'a' end") shouldBe a [Left[?, ?]]
    }

    it should "fail type checks when both sides have different array types" in {
        parseAndTypeCheckStr("begin int[] x = []; x = ['a'] end") shouldBe a [Left[?, ?]]
    }

    it should "fail type checks when RHS is an array with multiple types in it" in {
        parseAndTypeCheckStr("begin int[] x = []; x = [3, true] end") shouldBe a [Left[?, ?]]
    }
}
