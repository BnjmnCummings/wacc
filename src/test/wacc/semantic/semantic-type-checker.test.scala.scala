package test.wacc.semantic

import wacc.semantic.*
import wacc.*
import wacc.ast.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import wacc.semantic.Error.TypeMismatch

class types_tst extends AnyFlatSpec {
    "basic type declaration" should "be semantically valid" in {
        val funcs: List[Func] = List[Func]()

        val body: List[Stmt] = List[Stmt](
            Decl(BaseType.Int, Ident("x"), IntLiteral(7)),
            Decl(BaseType.Char, Ident("c"), CharLiteral('c')),
            Decl(BaseType.Bool, Ident("b"), BoolLiteral(true)),
            Decl(BaseType.String, Ident("s"), StringLiteral("Test"))
        )
        val prog: Prog = Prog(funcs, body)
        
        val tyInfo = TypeInfo(varTys = Map("x" -> KnownType.Int, "c" -> KnownType.Char, "b" -> KnownType.Boolean, "s" -> KnownType.String), funcTys = Map())

        val typedFuncs: List[TypedFunc] = List[TypedFunc]()

        val typedBody: List[TypedStmt] = List[TypedStmt](
            TypedStmt.Decl(BaseType.Int, TypedExpr.Ident("x"), TypedExpr.IntLiteral(7)),
            TypedStmt.Decl(BaseType.Char, TypedExpr.Ident("c"), TypedExpr.CharLiteral('c')),
            TypedStmt.Decl(BaseType.Bool, TypedExpr.Ident("b"), TypedExpr.BoolLiteral(true)),
            TypedStmt.Decl(BaseType.String, TypedExpr.Ident("s"), TypedExpr.StringLiteral("Test"))
        )

        wacc.semantic.typeCheck(prog, tyInfo) shouldBe Right(TypedProg(typedFuncs, typedBody))
    }

    "ints" should "be semantically invalid" in {
        val funcs: List[Func] = List[Func]()
        val body: List[Stmt] = List[Stmt](
            Decl(BaseType.Int, Ident("xInt"), StringLiteral("incorrect type")),
            Decl(BaseType.Int, Ident("yInt"), CharLiteral('z')),
            Decl(BaseType.Int, Ident("zInt"), BoolLiteral(true)),
        )

        val prog: Prog = Prog(funcs, body)
        
        val tyInfo = TypeInfo(varTys = Map(
            "xInt" -> KnownType.Int,
            "yInt" -> KnownType.Int,
            "zInt" -> KnownType.Int
        ), funcTys = Map())

        val typedFuncs: List[TypedFunc] = List[TypedFunc]()
        val typedBody: List[TypedStmt] = List[TypedStmt]()

        val expected = Left(List[Error](
            Error.TypeMismatch(KnownType.String, KnownType.Int),
            Error.TypeMismatch(KnownType.Char, KnownType.Int),
            Error.TypeMismatch(KnownType.Boolean, KnownType.Int)
        ))

        wacc.semantic.typeCheck(prog, tyInfo) shouldBe expected
    }

    "chars" should "be semantically invalid" in {
        val funcs: List[Func] = List[Func]()
        val body: List[Stmt] = List[Stmt](
            Decl(BaseType.Char, Ident("xChar"), StringLiteral("incorrect type")),
            Decl(BaseType.Char, Ident("yChar"), BoolLiteral(true)),
            Decl(BaseType.Char, Ident("zChar"), IntLiteral(4)),
        )

        val prog: Prog = Prog(funcs, body)
        
        val tyInfo = TypeInfo(varTys = Map(
            "xChar" -> KnownType.Char,
            "yChar" -> KnownType.Char,
            "zChar" -> KnownType.Char,
        ), funcTys = Map())

        val typedFuncs: List[TypedFunc] = List[TypedFunc]()
        val typedBody: List[TypedStmt] = List[TypedStmt]()

        val expected = Left(List[Error](
            Error.TypeMismatch(KnownType.String, KnownType.Char),
            Error.TypeMismatch(KnownType.Boolean, KnownType.Char),
            Error.TypeMismatch(KnownType.Int, KnownType.Char)
        ))

        wacc.semantic.typeCheck(prog, tyInfo) shouldBe expected
    }

    "strings" should "be semantically invalid" in {
        val funcs: List[Func] = List[Func]()
        val body: List[Stmt] = List[Stmt](
            Decl(BaseType.String, Ident("xStr"), CharLiteral('z')),
            Decl(BaseType.String, Ident("yStr"), BoolLiteral(true)),
            Decl(BaseType.String, Ident("zStr"), IntLiteral(4))
        )

        val prog: Prog = Prog(funcs, body)
        
        val tyInfo = TypeInfo(varTys = Map(
            "xStr" -> KnownType.String,
            "yStr" -> KnownType.String,
            "zStr" -> KnownType.String
        ), funcTys = Map())

        val typedFuncs: List[TypedFunc] = List[TypedFunc]()
        val typedBody: List[TypedStmt] = List[TypedStmt]()

        val expected = Left(List[Error](
            Error.TypeMismatch(KnownType.Char, KnownType.String),
            Error.TypeMismatch(KnownType.Boolean, KnownType.String),
            Error.TypeMismatch(KnownType.Int, KnownType.String),
        ))

        wacc.semantic.typeCheck(prog, tyInfo) shouldBe expected
    }

    "bools" should "be semantically invalid" in {
        val funcs: List[Func] = List[Func]()
        val body: List[Stmt] = List[Stmt](
            Decl(BaseType.Bool, Ident("xBool"), IntLiteral(4)),
            Decl(BaseType.Bool, Ident("xBool"), CharLiteral('z')),
            Decl(BaseType.Bool, Ident("xBool"), StringLiteral("incorrect type"))
        )

        val prog: Prog = Prog(funcs, body)
        
        val tyInfo = TypeInfo(varTys = Map(
            "xBool" -> KnownType.Boolean,
            "yBool" -> KnownType.Boolean,
            "zBool" -> KnownType.Boolean
        ), funcTys = Map())

        val typedFuncs: List[TypedFunc] = List[TypedFunc]()
        val typedBody: List[TypedStmt] = List[TypedStmt]()

        val expected = Left(List[Error](
            Error.TypeMismatch(KnownType.Int, KnownType.Boolean),
            Error.TypeMismatch(KnownType.Char, KnownType.Boolean),
            Error.TypeMismatch(KnownType.String, KnownType.Boolean)
        ))

        wacc.semantic.typeCheck(prog, tyInfo) shouldBe expected
    }
    
    "array declaration" should "be semantically valid" in {
        val funcs: List[Func] = List[Func]()
        val body: List[Stmt] = List[Stmt](
            Decl(ArrayType(BaseType.Int), Ident("intArr"), ArrayLiteral(List[Expr](IntLiteral(1), IntLiteral(2), IntLiteral(3))))
        )

        val prog: Prog = Prog(funcs, body)

        val tyInfo = TypeInfo(varTys = Map(
            "intArr" -> KnownType.Array(KnownType.Int)
        ), funcTys = Map())

        val typedFuncs: List[TypedFunc] = List[TypedFunc]()
        val typedBody: List[TypedStmt] = List[TypedStmt](
            TypedStmt.Decl(ArrayType(BaseType.Int), TypedExpr.Ident("intArr"), TypedRValue.ArrayLiteral(List[TypedExpr](TypedExpr.IntLiteral(1), TypedExpr.IntLiteral(2), TypedExpr.IntLiteral(3)), KnownType.Int))
        )

        val expected = Right(TypedProg(typedFuncs, typedBody))

        wacc.semantic.typeCheck(prog, tyInfo) shouldBe expected
    }
    
    "incorrect array declarations" should "be semantically invalid" in {
        val funcs: List[Func] = List[Func]()
        val body: List[Stmt] = List[Stmt](
            Decl(ArrayType(BaseType.Int), Ident("arr1"), IntLiteral(6)),
            Decl(ArrayType(BaseType.Int), Ident("arr2"), CharLiteral('b')),
            Decl(ArrayType(BaseType.Int), Ident("arr3"), BoolLiteral(false)),
            Decl(ArrayType(BaseType.Int), Ident("arr4"), ArrayLiteral(List[Expr](CharLiteral('a')))),
        )

        val prog: Prog = Prog(funcs, body)

        val tyInfo = TypeInfo(varTys = Map(
            "arr1" -> KnownType.Array(KnownType.Int),
            "arr2" -> KnownType.Array(KnownType.Int),
            "arr3" -> KnownType.Array(KnownType.Int),
            "arr4" -> KnownType.Array(KnownType.Int)
        ), funcTys = Map())

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

    "array declaration with mixed types" should "be semantically invalid" in {
        val funcs: List[Func] = List[Func]()
        val body: List[Stmt] = List[Stmt](
            Decl(ArrayType(BaseType.Int), Ident("arr5"), ArrayLiteral(List[Expr](IntLiteral(3), CharLiteral('a'))))
        )

        val prog: Prog = Prog(funcs, body)

        val tyInfo = TypeInfo(varTys = Map(
            "arr" -> KnownType.Array(KnownType.Int)
        ), funcTys = Map())

        val typedFuncs: List[TypedFunc] = List[TypedFunc]()
        val typedBody: List[TypedStmt] = List[TypedStmt]()

        val expected = Left(List(
            TypeMismatch(KnownType.Char, KnownType.Int)
        ))

        wacc.semantic.typeCheck(prog, tyInfo) shouldBe expected
    }
}