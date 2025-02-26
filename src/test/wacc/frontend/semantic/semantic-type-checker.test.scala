package wacc.semantic

import wacc.*
import wacc.q_ast.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

class types_test extends AnyFlatSpec {
    "basic type declaration" should "be semantically valid" in {
        val funcs: List[Q_Func] = List[Q_Func]()

        val body: List[Q_Stmt] = List[Q_Stmt](
            Q_Decl(Name("x", 0), Q_IntLiteral(7)),
            Q_Decl(Name("c", 0), Q_CharLiteral('c')),
            Q_Decl(Name("b", 0), Q_BoolLiteral(true)),
            Q_Decl(Name("s", 0), Q_StringLiteral("Test"))
        )

        val scoped: Set[Name] = Set[Name](Name("x", 0), Name("c", 0), Name("b", 0), Name("s", 0))

        val prog: Q_Prog = Q_Prog(funcs, body, scoped)
        
        val tyInfo = TypeInfo(varTys = Map(Name("x", 0) -> KnownType.Int, Name("c", 0) -> KnownType.Char, Name("b", 0) -> KnownType.Boolean, Name("s", 0) -> KnownType.String), funcTys = Map())

        wacc.semantic.typeCheck(prog, tyInfo) shouldBe a [Right[?, ?]]
    }

    "ints" should "be semantically invalid" in {
        val funcs: List[Q_Func] = List[Q_Func]()

        val body: List[Q_Stmt] = List[Q_Stmt](
            Q_Decl(Name("xInt", 0), Q_StringLiteral("incorrect type")),
            Q_Decl(Name("yInt", 0), Q_CharLiteral('z')),
            Q_Decl(Name("zInt", 0), Q_BoolLiteral(true))
        )

        val scoped: Set[Name] = Set[Name](Name("xInt", 0), Name("yInt", 0), Name("zInt", 0))

        val prog: Q_Prog = Q_Prog(funcs, body, scoped)

        val tyInfo = TypeInfo(varTys = Map(Name("xInt", 0) -> KnownType.Int, Name("yInt", 0) -> KnownType.Int, Name("zInt", 0) -> KnownType.Int), funcTys = Map())

        // val expected = Some(List[Err](
        //     TypeMismatch(KnownType.String, KnownType.Int),
        //     TypeMismatch(KnownType.Char, KnownType.Int),
        //     TypeMismatch(KnownType.Boolean, KnownType.Int)
        // ))

        wacc.semantic.typeCheck(prog, tyInfo) shouldBe a [Left[?, ?]]
    }

    "chars" should "be semantically invalid" in {
        val funcs: List[Q_Func] = List[Q_Func]()

        val body: List[Q_Stmt] = List[Q_Stmt](
            Q_Decl(Name("xChar", 0), Q_StringLiteral("incorrect type")),
            Q_Decl(Name("yChar", 0), Q_BoolLiteral(true)),
            Q_Decl(Name("zChar", 0), Q_IntLiteral(4))
        )

        val scoped: Set[Name] = Set[Name](Name("xChar", 0), Name("yChar", 0), Name("zChar", 0))

        val prog: Q_Prog = Q_Prog(funcs, body, scoped)
        
        val tyInfo = TypeInfo(varTys = Map(Name("xChar", 0) -> KnownType.Char, Name("yChar", 0) -> KnownType.Char, Name("zChar", 0) -> KnownType.Char), funcTys = Map())

        // val expected = Some(List[Error](
        //     TypeMismatch(KnownType.String, KnownType.Char),
        //     TypeMismatch(KnownType.Boolean, KnownType.Char),
        //     TypeMismatch(KnownType.Int, KnownType.Char)
        // ))

        wacc.semantic.typeCheck(prog, tyInfo) shouldBe a [Left[?, ?]]
    }

    "strings" should "be semantically invalid" in {
        val funcs: List[Q_Func] = List[Q_Func]()

        val body: List[Q_Stmt] = List[Q_Stmt](
            Q_Decl(Name("xStr", 0), Q_CharLiteral('z')),
            Q_Decl(Name("yStr", 0), Q_BoolLiteral(true)),
            Q_Decl(Name("zStr", 0), Q_IntLiteral(4))
        )

        val scoped: Set[Name] = Set[Name](Name("xStr", 0), Name("yStr", 0), Name("zStr", 0))

        val prog: Q_Prog = Q_Prog(funcs, body, scoped)
        
        val tyInfo = TypeInfo(varTys = Map(Name("xStr", 0) -> KnownType.String, Name("yStr", 0) -> KnownType.String, Name("zStr", 0) -> KnownType.String), funcTys = Map())

        // val expected = Some(List[Error](
        //     TypeMismatch(KnownType.Char, KnownType.String),
        //     TypeMismatch(KnownType.Boolean, KnownType.String),
        //     TypeMismatch(KnownType.Int, KnownType.String)
        // ))

        wacc.semantic.typeCheck(prog, tyInfo) shouldBe a [Left[?, ?]]
    }

    "bools" should "be semantically invalid" in {
        val funcs: List[Q_Func] = List[Q_Func]()

        val body: List[Q_Stmt] = List[Q_Stmt](
            Q_Decl(Name("xBool", 0), Q_IntLiteral(4)),
            Q_Decl(Name("yBool", 0), Q_CharLiteral('z')),
            Q_Decl(Name("zBool", 0), Q_StringLiteral("incorrect type"))
        )

        val scoped: Set[Name] = Set[Name](Name("xBool", 0), Name("yBool", 0), Name("zBool", 0))

        val prog: Q_Prog = Q_Prog(funcs, body, scoped)
        
        val tyInfo = TypeInfo(varTys = Map(Name("xBool", 0) -> KnownType.Boolean, Name("yBool", 0) -> KnownType.Boolean, Name("zBool", 0) -> KnownType.Boolean), funcTys = Map())

        // val expected = Some(List[Error](
        //     TypeMismatch(KnownType.Int, KnownType.Boolean),
        //     TypeMismatch(KnownType.Char, KnownType.Boolean),
        //     TypeMismatch(KnownType.String, KnownType.Boolean)
        // ))

        wacc.semantic.typeCheck(prog, tyInfo) shouldBe a [Left[?, ?]]
    }
    
    "array declaration" should "be semantically valid" in {
        val funcs: List[Q_Func] = List[Q_Func]()

        val body: List[Q_Stmt] = List[Q_Stmt](
            Q_Decl(Name("intArr", 0), Q_ArrayLiteral(List[Q_Expr](Q_IntLiteral(1), Q_IntLiteral(2), Q_IntLiteral(3))))
        )

        val scoped: Set[Name] = Set[Name](Name("intArr", 0))

        val prog: Q_Prog = Q_Prog(funcs, body, scoped)
        
        val tyInfo = TypeInfo(varTys = Map(Name("intArr", 0) -> KnownType.Array(KnownType.Int)), funcTys = Map())

        wacc.semantic.typeCheck(prog, tyInfo) shouldBe a [Right[?, ?]]
    }

    "incorrect array declarations" should "be semantically invalid" in {
        val funcs: List[Q_Func] = List[Q_Func]()

        val body: List[Q_Stmt] = List[Q_Stmt](
            Q_Decl(Name("arr1", 0), Q_IntLiteral(6)),
            Q_Decl(Name("arr2", 0), Q_CharLiteral('b')),
            Q_Decl(Name("arr3", 0), Q_BoolLiteral(false)),
            Q_Decl(Name("arr4", 0), Q_ArrayLiteral(List[Q_Expr](Q_CharLiteral('a'))))
            )

        val scoped: Set[Name] = Set[Name](Name("arr1", 0), Name("arr2", 0), Name("arr3", 0), Name("arr4", 0))

        val prog: Q_Prog = Q_Prog(funcs, body, scoped)
        
        val tyInfo = TypeInfo(varTys = Map(Name("arr1", 0) -> KnownType.Array(KnownType.Int), Name("arr2", 0) -> KnownType.Array(KnownType.Int), Name("arr3", 0) -> KnownType.Array(KnownType.Int), Name("arr4", 0) -> KnownType.Array(KnownType.Int)), funcTys = Map())

        // val expected = Some(List(
        //     TypeMismatch(KnownType.Int, KnownType.Array(KnownType.Int)),
        //     TypeMismatch(KnownType.Char, KnownType.Array(KnownType.Int)),
        //     TypeMismatch(KnownType.Boolean, KnownType.Array(KnownType.Int)),
        //     TypeMismatch(KnownType.Array(KnownType.Char), KnownType.Array(KnownType.Int))
        // ))

        wacc.semantic.typeCheck(prog, tyInfo) shouldBe a [Left[?, ?]]
    }

    /*"array declaration with mixed types" should "be semantically invalid" in {
        val funcs: List[Q_Func] = List[Q_Func]()

        val body: List[Q_Stmt] = List[Q_Stmt](
            Q_Decl(Name("arr5", 0), Q_ArrayLiteral(List[Q_Expr](Q_IntLiteral(3), Q_CharLiteral('a'))))
        )

        val scoped: Set[Name] = Set[Name](Name("arr5", 0))

        val prog: Q_Prog = Q_Prog(funcs, body, scoped)
        
        val tyInfo = TypeInfo(varTys = Map(Name("arr5", 0) -> KnownType.Array(KnownType.Int)), funcTys = Map())

        val expected = Left(List(
            TypeMismatch(KnownType.Char, KnownType.Int)
        ))

        wacc.semantic.typeCheck(prog, tyInfo) shouldBe expected
    }*/
}

class types_test2 extends AnyFlatSpec {
    "free" should "be able to free arrays" in {
        val funcs: List[Q_Func] = List[Q_Func]()

        val body: List[Q_Stmt] = List[Q_Stmt](
            Q_Decl(Name("arr1", 0), Q_ArrayLiteral(List[Q_Expr](Q_IntLiteral(1)))),
            Q_Free(Q_Ident(Name("arr1", 0)))
            )

        val scoped: Set[Name] = Set[Name](Name("arr1", 0))

        val prog: Q_Prog = Q_Prog(funcs, body, scoped)
        
        val tyInfo = TypeInfo(varTys = Map(Name("arr1", 0) -> KnownType.Array(KnownType.Int)), funcTys = Map())

        wacc.semantic.typeCheck(prog, tyInfo) shouldBe a [Right[?, ?]]
    }

    "free" should "be able to free pairs" in {
        val funcs: List[Q_Func] = List[Q_Func]()

        val body: List[Q_Stmt] = List[Q_Stmt](
            Q_Decl(Name("p", 0), Q_NewPair(Q_IntLiteral(1), Q_IntLiteral(2))),
            Q_Free(Q_Ident(Name("p", 0)))
            )

        val scoped: Set[Name] = Set[Name](Name("p", 0))

        val prog: Q_Prog = Q_Prog(funcs, body, scoped)
        
        val tyInfo = TypeInfo(varTys = Map(Name("p", 0) -> KnownType.Pair(KnownType.Int, KnownType.Int)), funcTys = Map())

        wacc.semantic.typeCheck(prog, tyInfo) shouldBe a [Right[?, ?]]
    }

    it should "reject strings" in {
        parseAndTypeCheckStr("begin string s = \"adam marshall\"; free s end") `shouldBe` a [Left[?, ?]]
    }

    "type checker" should "allow char[] to take the place of string" in {
        parseAndTypeCheckStr("begin char[] s = [\'a\']; string s2 = s end") `shouldBe` a [Right[?, ?]]
    }

    it should "not allow string to take the place of char[]" in {
        parseAndTypeCheckStr("begin string s = \"a\"; char[] s2 = s end") `shouldBe` a [Left[?, ?]]
    }

    it should "not allow char[][] to take the place of string[]" in {
        parseAndTypeCheckStr("begin char[] s0 = [\'a\']; char[][] s = [s0]; string[] s2 = s end") `shouldBe` a [Left[?, ?]]
    }
    it should "allow a char[] to be put in a string[]" in {
        parseAndTypeCheckStr("begin char[] s0 = [\'a\']; string[] s2 = [s0] end") `shouldBe` a [Right[?, ?]]
    }

    it should "allow nested pairs" in {
        parseAndTypeCheckStr("begin pair(int, int) p = newpair(1, 2); pair(pair, pair) nestedPair = newpair(p, p) end") `shouldBe` a [Right[?, ?]]
    }

    "return" should "not be allowed outside function bodies" in {
        parseAndTypeCheckStr("begin return 7 end") `shouldBe` a [Left[?, ?]]
    }

    it should "type check successfully with the correct type inside a function body" in {
        parseAndTypeCheckStr("begin int x() is return 7 end skip end") `shouldBe` a [Right[?, ?]]
    }

    it should "fail when the incorrect type is returned" in {
        parseAndTypeCheckStr("begin int x() is return \'a\' end skip end") `shouldBe` a [Left[?, ?]]
    }

    "read" should "accept an integer value" in {
        parseAndTypeCheckStr("begin int x = 0; read x end") `shouldBe` a [Right[?, ?]]
    }

    it should "accept a char value" in {
        parseAndTypeCheckStr("begin char a = 'a'; read a end") `shouldBe` a [Right[?, ?]]
    }

    it should "reject any other types" in {
        parseAndTypeCheckStr("begin string a = \"a\"; read a end") `shouldBe` a [Left[?, ?]]
        parseAndTypeCheckStr("begin bool a = true; read a end") `shouldBe` a [Left[?, ?]]
        parseAndTypeCheckStr("begin int[] a = [9]; read a end") `shouldBe` a [Left[?, ?]]
    }

    "exit" should "accept an integer value" in {
        parseAndTypeCheckStr("begin exit 0 end") `shouldBe` a [Right[?, ?]]
        parseAndTypeCheckStr("begin exit 3 + 4 end") `shouldBe` a [Right[?, ?]]
    }
    
    it should "reject any other type" in {
        parseAndTypeCheckStr("begin exit \'a\' end") `shouldBe` a [Left[?, ?]]
        parseAndTypeCheckStr("begin exit true end") `shouldBe` a [Left[?, ?]]
    }

    "if statement" should "accept a boolean condition" in {
        parseAndTypeCheckStr("begin if (3 == 3) then skip else skip fi end") `shouldBe` a [Right[?, ?]]
    }

    it should "reject any other type of condition" in {
        parseAndTypeCheckStr("begin if (3 + 3) then skip else skip fi end") `shouldBe` a [Left[?, ?]]
    }

    "while loop" should "accept a boolean condition" in {
        parseAndTypeCheckStr("begin while (3 == 3) do skip done end") `shouldBe` a [Right[?, ?]]
    }

    it should "reject any other type of condition" in {
        parseAndTypeCheckStr("begin while (3 + 3) do skip done end") `shouldBe` a [Left[?, ?]]
    }

    "declaration" should "pass type checks when both sides are int type" in {
        parseAndTypeCheckStr("begin int x = 7 end") `shouldBe` a [Right[?, ?]]
        parseAndTypeCheckStr("begin int x = 7 + 7 end") `shouldBe` a [Right[?, ?]]
    }

    it should "pass type checks when both sides are array type" in {
        parseAndTypeCheckStr("begin int[] x = [1, 2] end") `shouldBe` a [Right[?, ?]]
        parseAndTypeCheckStr("begin int[] x = [1, 2 + 3] end") `shouldBe` a [Right[?, ?]]
    }

    it should "fail type checks when both sides have different types" in {
        parseAndTypeCheckStr("begin int x = \'a\' end") `shouldBe` a [Left[?, ?]]
    }

    it should "fail type checks when both sides have different array types" in {
        parseAndTypeCheckStr("begin int[] x = [\'a\'] end") `shouldBe` a [Left[?, ?]]
    }

    it should "fail type checks when RHS is an array with multiple types in it" in {
        parseAndTypeCheckStr("begin int[] x = [9, true] end") `shouldBe` a [Left[?, ?]]
    }

    "assignment" should "pass type checks when both sides are int type" in {
        parseAndTypeCheckStr("begin int x = 7; x = 0 end") `shouldBe` a [Right[?, ?]]
        parseAndTypeCheckStr("begin int x = 7 + 7; x = 0 end") `shouldBe` a [Right[?, ?]]
    }

    it should "pass type checks when both sides are array type" in {
        parseAndTypeCheckStr("begin int[] x = [1, 2]; x = [1] end") `shouldBe` a [Right[?, ?]]
        parseAndTypeCheckStr("begin int[] x = [1, 2]; x = [1 + 1] end") `shouldBe` a [Right[?, ?]]
    }

    it should "fail type checks when both sides have different types" in {
        parseAndTypeCheckStr("begin int x = 3; x = 'a' end") `shouldBe` a [Left[?, ?]]
    }

    it should "fail type checks when both sides have different array types" in {
        parseAndTypeCheckStr("begin int[] x = []; x = ['a'] end") `shouldBe` a [Left[?, ?]]
    }

    it should "fail type checks when RHS is an array with multiple types in it" in {
        parseAndTypeCheckStr("begin int[] x = []; x = [3, true] end") `shouldBe` a [Left[?, ?]]
    }
}