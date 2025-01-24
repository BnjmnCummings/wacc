package wacc.syntax

import wacc.parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import parsley.{Success, Failure, Result}

class type_array_test extends AnyFlatSpec {
    "arrayType" should "be able to parse basic array types" in {
        parser.arrayType.parse("int[]")    shouldBe Success(ArrayType(BaseType.Int))
        parser.arrayType.parse("bool[]")   shouldBe Success(ArrayType(BaseType.Bool))
        parser.arrayType.parse("char[]")   shouldBe Success(ArrayType(BaseType.Char))
        parser.arrayType.parse("string[]") shouldBe Success(ArrayType(BaseType.String))
    }

    it should "be able to parse nested array types" in {
        parser.arrayType.parse("int[][]")    shouldBe Success(
            ArrayType(
                ArrayType(BaseType.Int)
            )
        )
        parser.arrayType.parse("bool[][][]") shouldBe Success(
            ArrayType(
                ArrayType(
                    ArrayType(BaseType.Bool)
                )
            )
        )
    }

    it should "be able to parse arrays of pairs" in {
        parser.arrayType.parse("pair(int, int)[]") shouldBe Success(
            ArrayType(
                PairType(BaseType.Int, BaseType.Int)
            )
        )
        parser.arrayType.parse("pair(bool, string)[][]") shouldBe Success(
            ArrayType(
                ArrayType(
                    PairType(BaseType.Bool, BaseType.String)
                )
            )
        )

    }

    it should "fail non existent array types" in {
        parser.arrayType.parse("intint[]")  shouldBe a [Failure[?]]
        parser.arrayType.parse("stringg[]") shouldBe a [Failure[?]]
        parser.arrayType.parse("cheese[]")  shouldBe a [Failure[?]]
    }
}

class type_pair_test extends AnyFlatSpec {
    "pairType" should "be able to parse basic pair types" in {
        parser.pairType.parse("pair(int, int)")       shouldBe Success(PairType(BaseType.Int, BaseType.Int))
        parser.pairType.parse("pair(bool, bool)")     shouldBe Success(PairType(BaseType.Bool, BaseType.Bool))
        parser.pairType.parse("pair(char, char)")     shouldBe Success(PairType(BaseType.Char, BaseType.Char))
        parser.pairType.parse("pair(string, string)") shouldBe Success(PairType(BaseType.String, BaseType.String))
    }

    it should "be able to parse differing pair types" in {
        parser.pairType.parse("pair(int, bool)")    shouldBe Success(PairType(BaseType.Int, BaseType.Bool))
        parser.pairType.parse("pair(bool, int)")    shouldBe Success(PairType(BaseType.Bool, BaseType.Int))
        parser.pairType.parse("pair(char, string)") shouldBe Success(PairType(BaseType.Char, BaseType.String))
    }

    it should "be able to parse nested pair types" in {
       parser.pairType.parse("pair(pair, pair)") shouldBe Success(PairType(ErasedPairType, ErasedPairType))
       parser.pairType.parse("pair(int, pair)") shouldBe Success(PairType(BaseType.Int, ErasedPairType))

    }

    it should "be able to parse pairs of arrays" in {
        parser.pairType.parse("pair(int, int[])") shouldBe Success(
            PairType(
                BaseType.Int, 
                ArrayType(BaseType.Int)
            )
        )
        parser.pairType.parse("pair(pair(int, int)[], string)") shouldBe Success(
            PairType(
                ArrayType(PairType(BaseType.Int, BaseType.Int)), 
                BaseType.String
            )
        )
        parser.pairType.parse("pair(pair(int, int)[][], pair)") shouldBe Success(
            PairType(
                ArrayType(
                    ArrayType(
                        PairType(BaseType.Int, BaseType.Int)
                    )
                ), 
                ErasedPairType
            )
        )
    }

    it should "fail non existent pair types" in {
        parser.pairType.parse("int(pair, pair)")  shouldBe a [Failure[?]]
        parser.pairType.parse("pair(intywinty, boolywooly)") shouldBe a [Failure[?]]
        parser.pairType.parse("pair(,)")  shouldBe a [Failure[?]]
    }
}

class type_base_test extends AnyFlatSpec {
    "baseType" should "be able to parse valid base types" in {
        parser.baseType.parse("int")    shouldBe Success(BaseType.Int)
        parser.baseType.parse("bool")   shouldBe Success(BaseType.Bool)
        parser.baseType.parse("char")   shouldBe Success(BaseType.Char)
        parser.baseType.parse("string") shouldBe Success(BaseType.String)
    }

    it should "fail non existent types" in {
        parser.baseType.parse("spoon")        shouldBe a [Failure[?]]
        parser.baseType.parse("notARealType") shouldBe a [Failure[?]]
        parser.baseType.parse("helpMe")       shouldBe a [Failure[?]]
        parser.baseType.parse("cheese")       shouldBe a [Failure[?]]
    }

    
}

class type_parent_test extends AnyFlatSpec {
    "_type" should "be able to parse valid base types" in {
        parser._type.parse("int")    shouldBe Success(BaseType.Int)
        parser._type.parse("bool")   shouldBe Success(BaseType.Bool)
        parser._type.parse("char")   shouldBe Success(BaseType.Char)
        parser._type.parse("string") shouldBe Success(BaseType.String)
    }

    it should "be able to parse pairs of arrays" in {
        parser._type.parse("pair(int, int[])") shouldBe Success(
            PairType(
                BaseType.Int, 
                ArrayType(BaseType.Int)
            )
        )
        parser._type.parse("pair(pair(int, int)[], string)") shouldBe Success(
            PairType(
                ArrayType(PairType(BaseType.Int, BaseType.Int)), 
                BaseType.String
            )
        )
        parser._type.parse("pair(pair(int, int)[][], pair)") shouldBe Success(
            PairType(
                ArrayType(
                    ArrayType(
                        PairType(BaseType.Int, BaseType.Int)
                    )
                ), 
                ErasedPairType
            )
        )
    }

    it should "be able to parse arrays of pairs" in {
        parser._type.parse("pair(int, int)[]") shouldBe Success(
            ArrayType(
                PairType(BaseType.Int, BaseType.Int)
            )
        )
        parser._type.parse("pair(bool, string)[][]") shouldBe Success(
            ArrayType(
                ArrayType(
                    PairType(BaseType.Bool, BaseType.String)
                )
            )
        )
    }
}
