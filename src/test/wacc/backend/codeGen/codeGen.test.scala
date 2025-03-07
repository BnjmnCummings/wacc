package wacc.codeGen

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import wacc.t_ast.*
import wacc.q_ast.Name
import wacc.assemblyIR.*
import wacc.TypeInfo
import wacc.KnownType

class code_gen_test extends AnyFlatSpec {
    "genIntLiteral" should "generate correct assembly for integer literals" in {
        given ctx: CodeGenCtx = CodeGenCtx(
            TypeInfo(
                Map(), 
                Map()
            ), 
            TableCtx(
                StackTables(None)
            )
        )
        val result = genIntLiteral(42)
        result shouldBe List(A_MovTo(A_Reg(A_RegName.RetReg), A_Imm(42), INT_SIZE))
    }

    "genBoolLiteral" should "generate correct assembly for boolean literals" in {
        given ctx: CodeGenCtx = CodeGenCtx(
            TypeInfo(
                Map(), 
                Map()
            ), 
            TableCtx(
                StackTables(None)
            )
        )
        val resultTrue = genBoolLiteral(true)
        val resultFalse = genBoolLiteral(false)
        resultTrue shouldBe List(A_MovTo(A_Reg(A_RegName.RetReg), A_Imm(TRUE), BOOL_SIZE))
        resultFalse shouldBe List(A_MovTo(A_Reg(A_RegName.RetReg), A_Imm(FALSE), BOOL_SIZE))
    }

    "genCharLiteral" should "generate correct assembly for char literals" in {
        given ctx: CodeGenCtx = CodeGenCtx(
            TypeInfo(
                Map(), 
                Map()
            ), 
            TableCtx(
                StackTables(None)
            )
        )
        val result = genCharLiteral('a')
        result shouldBe List(A_MovTo(A_Reg(A_RegName.RetReg), A_Imm('a'.toInt), CHAR_SIZE))
    }

    "genStringLiteral" should "generate correct assembly for string literals" in {
        given ctx: CodeGenCtx = CodeGenCtx(
            TypeInfo(
                Map(), 
                Map()
            ), 
            TableCtx(
                StackTables(None)
            )
        )
        val result = genStringLiteral("hello")
        result.nonEmpty shouldBe true
    }

    "genIdent" should "generate correct assembly for identifiers" in {
        given ctx: CodeGenCtx = CodeGenCtx(
            TypeInfo(
                Map(Name("x", 0) -> KnownType.Int), 
                Map()
            ), 
            TableCtx(
                StackTables(None)
            )
        )

        val stackTable = StackTable(0)
        stackTable.addScope(
            Set(Name("x", 0)), 
            ctx.typeInfo
        )
        val stackTables = StackTables(Some(stackTable))
        val result = genIdent(Name("x", 0), stackTables)
        result shouldBe List(
            A_MovTo(
                A_Reg(A_RegName.RetReg), 
                A_RegDeref(A_MemOffset(A_Reg(A_RegName.BasePtr), A_OffsetImm(16))), 
                INT_SIZE
            )
        )
    }

    "genAddSub" should "generate correct assembly for addition and subtraction" in {
        given ctx: CodeGenCtx = CodeGenCtx(
            TypeInfo(
                Map(), 
                Map()
            ), 
            TableCtx(
                StackTables(None)
            )
        )
        val stackTable = StackTables(None)
        val resultAdd = genAddSub(T_IntLiteral(1), T_IntLiteral(2), A_Add.apply, stackTable)
        val resultSub = genAddSub(T_IntLiteral(3), T_IntLiteral(1), A_Sub.apply, stackTable)
        resultAdd.nonEmpty shouldBe true
        resultSub.nonEmpty shouldBe true
    }
}
