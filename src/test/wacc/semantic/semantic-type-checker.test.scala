package test.wacc.semantic

import wacc.semantic.*
import wacc.*
import wacc.ast.*
import wacc.q_ast.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import wacc.semantic.Error.TypeMismatch
import parsley.Failure
import parsley.Success

class types_tst extends AnyFlatSpec {
    "basic type declaration" should "be semantically valid" in {
        val funcs: List[Q_Func] = List[Q_Func]()

        val body: List[Q_Stmt] = List[Q_Stmt](
            Q_Decl(Q_Name("x", 0), Q_IntLiteral(7)),
            Q_Decl(Q_Name("c", 0), Q_CharLiteral('c')),
            Q_Decl(Q_Name("b", 0), Q_BoolLiteral(true)),
            Q_Decl(Q_Name("s", 0), Q_StringLiteral("Test"))
        )

        val scoped: Set[Q_Name] = Set[Q_Name](Q_Name("x", 0), Q_Name("c", 0), Q_Name("b", 0), Q_Name("s", 0))

        val prog: Q_Prog = Q_Prog(funcs, body, scoped)
        
        val tyInfo = TypeInfo(varTys = Map(Q_Name("x", 0) -> KnownType.Int, Q_Name("c", 0) -> KnownType.Char, Q_Name("b", 0) -> KnownType.Boolean, Q_Name("s", 0) -> KnownType.String), funcTys = Map())

        val typedFuncs: List[TypedFunc] = List[TypedFunc]()

        val typedBody: List[TypedStmt] = List[TypedStmt](
            TypedStmt.Decl(TypedExpr.Ident(Q_Name("x", 0)), TypedExpr.IntLiteral(7)),
            TypedStmt.Decl(TypedExpr.Ident(Q_Name("c", 0)), TypedExpr.CharLiteral('c')),
            TypedStmt.Decl(TypedExpr.Ident(Q_Name("b", 0)), TypedExpr.BoolLiteral(true)),
            TypedStmt.Decl(TypedExpr.Ident(Q_Name("s", 0)), TypedExpr.StringLiteral("Test"))
        )

        wacc.semantic.typeCheck(prog, tyInfo) shouldBe Right(TypedProg(typedFuncs, typedBody))
    }

    "ints" should "be semantically invalid" in {
        val funcs: List[Q_Func] = List[Q_Func]()

        val body: List[Q_Stmt] = List[Q_Stmt](
            Q_Decl(Q_Name("xInt", 0), Q_StringLiteral("incorrect type")),
            Q_Decl(Q_Name("yInt", 0), Q_CharLiteral('z')),
            Q_Decl(Q_Name("zInt", 0), Q_BoolLiteral(true))
        )

        val scoped: Set[Q_Name] = Set[Q_Name](Q_Name("xInt", 0), Q_Name("yInt", 0), Q_Name("zInt", 0))

        val prog: Q_Prog = Q_Prog(funcs, body, scoped)

        val tyInfo = TypeInfo(varTys = Map(Q_Name("xInt", 0) -> KnownType.Int, Q_Name("yInt", 0) -> KnownType.Int, Q_Name("zInt", 0) -> KnownType.Int), funcTys = Map())

        val expected = Left(List[Error](
            Error.TypeMismatch(KnownType.String, KnownType.Int),
            Error.TypeMismatch(KnownType.Char, KnownType.Int),
            Error.TypeMismatch(KnownType.Boolean, KnownType.Int)
        ))

        wacc.semantic.typeCheck(prog, tyInfo) shouldBe expected
    }

    "chars" should "be semantically invalid" in {
        val funcs: List[Q_Func] = List[Q_Func]()

        val body: List[Q_Stmt] = List[Q_Stmt](
            Q_Decl(Q_Name("xChar", 0), Q_StringLiteral("incorrect type")),
            Q_Decl(Q_Name("yChar", 0), Q_BoolLiteral(true)),
            Q_Decl(Q_Name("zChar", 0), Q_IntLiteral(4))
        )

        val scoped: Set[Q_Name] = Set[Q_Name](Q_Name("xChar", 0), Q_Name("yChar", 0), Q_Name("zChar", 0))

        val prog: Q_Prog = Q_Prog(funcs, body, scoped)
        
        val tyInfo = TypeInfo(varTys = Map(Q_Name("xChar", 0) -> KnownType.Char, Q_Name("yChar", 0) -> KnownType.Char, Q_Name("zChar", 0) -> KnownType.Char), funcTys = Map())

        val expected = Left(List[Error](
            Error.TypeMismatch(KnownType.String, KnownType.Char),
            Error.TypeMismatch(KnownType.Boolean, KnownType.Char),
            Error.TypeMismatch(KnownType.Int, KnownType.Char)
        ))

        wacc.semantic.typeCheck(prog, tyInfo) shouldBe expected
    }

    "strings" should "be semantically invalid" in {
        val funcs: List[Q_Func] = List[Q_Func]()

        val body: List[Q_Stmt] = List[Q_Stmt](
            Q_Decl(Q_Name("xStr", 0), Q_CharLiteral('z')),
            Q_Decl(Q_Name("yStr", 0), Q_BoolLiteral(true)),
            Q_Decl(Q_Name("zStr", 0), Q_IntLiteral(4))
        )

        val scoped: Set[Q_Name] = Set[Q_Name](Q_Name("xStr", 0), Q_Name("yStr", 0), Q_Name("zStr", 0))

        val prog: Q_Prog = Q_Prog(funcs, body, scoped)
        
        val tyInfo = TypeInfo(varTys = Map(Q_Name("xStr", 0) -> KnownType.String, Q_Name("yStr", 0) -> KnownType.String, Q_Name("zStr", 0) -> KnownType.String), funcTys = Map())

        val expected = Left(List[Error](
            Error.TypeMismatch(KnownType.Char, KnownType.String),
            Error.TypeMismatch(KnownType.Boolean, KnownType.String),
            Error.TypeMismatch(KnownType.Int, KnownType.String)
        ))

        wacc.semantic.typeCheck(prog, tyInfo) shouldBe expected
    }

    "bools" should "be semantically invalid" in {
        val funcs: List[Q_Func] = List[Q_Func]()

        val body: List[Q_Stmt] = List[Q_Stmt](
            Q_Decl(Q_Name("xBool", 0), Q_IntLiteral(4)),
            Q_Decl(Q_Name("yBool", 0), Q_CharLiteral('z')),
            Q_Decl(Q_Name("zBool", 0), Q_StringLiteral("incorrect type"))
        )

        val scoped: Set[Q_Name] = Set[Q_Name](Q_Name("xBool", 0), Q_Name("yBool", 0), Q_Name("zBool", 0))

        val prog: Q_Prog = Q_Prog(funcs, body, scoped)
        
        val tyInfo = TypeInfo(varTys = Map(Q_Name("xBool", 0) -> KnownType.Boolean, Q_Name("yBool", 0) -> KnownType.Boolean, Q_Name("zBool", 0) -> KnownType.Boolean), funcTys = Map())

        val expected = Left(List[Error](
            Error.TypeMismatch(KnownType.Int, KnownType.Boolean),
            Error.TypeMismatch(KnownType.Char, KnownType.Boolean),
            Error.TypeMismatch(KnownType.String, KnownType.Boolean)
        ))

        wacc.semantic.typeCheck(prog, tyInfo) shouldBe expected
    }
    
    "array declaration" should "be semantically valid" in {
        val funcs: List[Q_Func] = List[Q_Func]()

        val body: List[Q_Stmt] = List[Q_Stmt](
            Q_Decl(Q_Name("intArr", 0), Q_ArrayLiteral(List[Q_Expr](Q_IntLiteral(1), Q_IntLiteral(2), Q_IntLiteral(3))))
        )

        val scoped: Set[Q_Name] = Set[Q_Name](Q_Name("intArr", 0))

        val prog: Q_Prog = Q_Prog(funcs, body, scoped)
        
        val tyInfo = TypeInfo(varTys = Map(Q_Name("intArr", 0) -> KnownType.Array(KnownType.Int)), funcTys = Map())

        val typedFuncs: List[TypedFunc] = List[TypedFunc]()
        val typedBody: List[TypedStmt] = List[TypedStmt](
            TypedStmt.Decl(TypedExpr.Ident(Q_Name("intArr", 0)), TypedRValue.ArrayLiteral(List[TypedExpr](TypedExpr.IntLiteral(1), TypedExpr.IntLiteral(2), TypedExpr.IntLiteral(3)), KnownType.Int))
        )

        wacc.semantic.typeCheck(prog, tyInfo) shouldBe Right(TypedProg(typedFuncs, typedBody))
    }

    "incorrect array declarations" should "be semantically invalid" in {
        val funcs: List[Q_Func] = List[Q_Func]()

        val body: List[Q_Stmt] = List[Q_Stmt](
            Q_Decl(Q_Name("arr1", 0), Q_IntLiteral(6)),
            Q_Decl(Q_Name("arr2", 0), Q_CharLiteral('b')),
            Q_Decl(Q_Name("arr3", 0), Q_BoolLiteral(false)),
            Q_Decl(Q_Name("arr4", 0), Q_ArrayLiteral(List[Q_Expr](Q_CharLiteral('a'))))
            )

        val scoped: Set[Q_Name] = Set[Q_Name](Q_Name("arr1", 0), Q_Name("arr2", 0), Q_Name("arr3", 0), Q_Name("arr4", 0))

        val prog: Q_Prog = Q_Prog(funcs, body, scoped)
        
        val tyInfo = TypeInfo(varTys = Map(Q_Name("arr1", 0) -> KnownType.Array(KnownType.Int), Q_Name("arr2", 0) -> KnownType.Array(KnownType.Int), Q_Name("arr3", 0) -> KnownType.Array(KnownType.Int), Q_Name("arr4", 0) -> KnownType.Array(KnownType.Int)), funcTys = Map())

        val typedFuncs: List[TypedFunc] = List[TypedFunc]()
        val typedBody: List[TypedStmt] = List[TypedStmt]()

        val expected = Left(List(
            TypeMismatch(KnownType.Int, KnownType.Array(KnownType.Int)),
            TypeMismatch(KnownType.Char, KnownType.Array(KnownType.Int)),
            TypeMismatch(KnownType.Boolean, KnownType.Array(KnownType.Int)),
            TypeMismatch(KnownType.Array(KnownType.Char), KnownType.Array(KnownType.Int))
        ))

        wacc.semantic.typeCheck(prog, tyInfo) shouldBe expected
    }

    /*"array declaration with mixed types" should "be semantically invalid" in {
        val funcs: List[Q_Func] = List[Q_Func]()

        val body: List[Q_Stmt] = List[Q_Stmt](
            Q_Decl(Q_Name("arr5", 0), Q_ArrayLiteral(List[Q_Expr](Q_IntLiteral(3), Q_CharLiteral('a'))))
        )

        val scoped: Set[Q_Name] = Set[Q_Name](Q_Name("arr5", 0))

        val prog: Q_Prog = Q_Prog(funcs, body, scoped)
        
        val tyInfo = TypeInfo(varTys = Map(Q_Name("arr5", 0) -> KnownType.Array(KnownType.Int)), funcTys = Map())

        val expected = Left(List(
            TypeMismatch(KnownType.Char, KnownType.Int)
        ))

        wacc.semantic.typeCheck(prog, tyInfo) shouldBe expected
    }*/

    "free" should "be able to free arrays" in {
        val funcs: List[Q_Func] = List[Q_Func]()

        val body: List[Q_Stmt] = List[Q_Stmt](
            Q_Decl(Q_Name("arr1", 0), Q_ArrayLiteral(List[Q_Expr](Q_IntLiteral(1)))),
            Q_Free(Q_Ident(Q_Name("arr1", 0)))
            )

        val scoped: Set[Q_Name] = Set[Q_Name](Q_Name("arr1", 0))

        val prog: Q_Prog = Q_Prog(funcs, body, scoped)
        
        val tyInfo = TypeInfo(varTys = Map(Q_Name("arr1", 0) -> KnownType.Array(KnownType.Int)), funcTys = Map())

        val typedFuncs: List[TypedFunc] = List[TypedFunc]()
        val typedBody: List[TypedStmt] = List[TypedStmt] (
            TypedStmt.Decl(TypedExpr.Ident(Q_Name("arr1", 0)), TypedRValue.ArrayLiteral(List[TypedExpr](TypedExpr.IntLiteral(1)), KnownType.Int)),
            TypedStmt.Free(TypedExpr.Ident(Q_Name("arr1", 0)))
        )

        wacc.semantic.typeCheck(prog, tyInfo) shouldBe Right(TypedProg(typedFuncs, typedBody))
    }

    "free" should "be able to free pairs" in {
        val funcs: List[Q_Func] = List[Q_Func]()

        val body: List[Q_Stmt] = List[Q_Stmt](
            Q_Decl(Q_Name("p", 0), Q_NewPair(Q_IntLiteral(1), Q_IntLiteral(2))),
            Q_Free(Q_Ident(Q_Name("p", 0)))
            )

        val scoped: Set[Q_Name] = Set[Q_Name](Q_Name("p", 0))

        val prog: Q_Prog = Q_Prog(funcs, body, scoped)
        
        val tyInfo = TypeInfo(varTys = Map(Q_Name("p", 0) -> KnownType.Pair(KnownType.Int, KnownType.Int)), funcTys = Map())

        val typedFuncs: List[TypedFunc] = List[TypedFunc]()
        val typedBody: List[TypedStmt] = List[TypedStmt] (
            TypedStmt.Decl(TypedExpr.Ident(Q_Name("p", 0)), TypedRValue.NewPair(TypedExpr.IntLiteral(1), TypedExpr.IntLiteral(2))),
            TypedStmt.Free(TypedExpr.Ident(Q_Name("p", 0)))
        )

        wacc.semantic.typeCheck(prog, tyInfo) shouldBe Right(TypedProg(typedFuncs, typedBody))
    }

    "free" should "reject strings" in {
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
}