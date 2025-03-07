package wacc.formatting

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import java.io.StringWriter

import wacc.assemblyIR.*

class format_arithmetic_op_test extends AnyFlatSpec {
    "formatInstr" should "be able to format an add instruction" in {
        val writer = new StringWriter()
        formatInstr(A_Add(A_Reg(A_RegName.RetReg), A_Imm(5), INT_SIZE))(using writer)
        writer.toString should be ("add eax, 5")
    }

    it should "be able to format a sub instruction" in {
        val writer = new StringWriter()
        formatInstr(A_Sub(A_Reg(A_RegName.RetReg), A_Imm(5), INT_SIZE))(using writer)
        writer.toString should be ("sub eax, 5")
    }

    // it should "be able to format a mul instruction" in {
    //     val writer = new StringWriter()
    //     formatInstr(A_Mul(A_Reg(INT_SIZE, A_RegName.RetReg), A_Imm(5), INT_SIZE))(using writer)
    //     writer.toString should be ("mul eax, 5")
    // }

    it should "be able to format a div instruction" in {
        val writer = new StringWriter()
        formatInstr(A_Div(A_Reg(A_RegName.RetReg), A_Imm(5), INT_SIZE))(using writer)
        writer.toString should be ("div eax, 5")
    }
}

class format_branching_test extends AnyFlatSpec {
    "formatInstr" should "be able to format a cmp instruction" in {
        val writer = new StringWriter()
        formatInstr(A_Cmp(A_Reg(A_RegName.RetReg), A_Imm(5), INT_SIZE))(using writer)
        writer.toString should be ("cmp eax, 5")
    }

    it should "be able to format a jmp instruction" in {
        val writer = new StringWriter()
        formatInstr(A_Jmp(A_InstrLabel(".L0"), A_Cond.Eq))(using writer)
        writer.toString should be ("je .L0")

        val writer2 = new StringWriter()
        formatInstr(A_Jmp(A_InstrLabel(".L0"), A_Cond.Uncond))(using writer2)
        writer2.toString should be ("jmp .L0")
    }

    it should "be able to format a label start instruction" in {
        val writer = new StringWriter()
        formatInstr(A_LabelStart(A_InstrLabel(".L0")))(using writer)
        writer.toString should be (".L0:")
    }
}

class format_memory_ops_test extends AnyFlatSpec {
    "formatInstr" should "be able to format a push instruction" in {
        val writer = new StringWriter()
        formatInstr(A_Push(A_Reg(A_RegName.RetReg)))(using writer)
        writer.toString should be ("push rax")
    }

    it should "be able to format a pop instruction" in {
        val writer = new StringWriter()
        formatInstr(A_Pop(A_Reg(A_RegName.RetReg)))(using writer)
        writer.toString should be ("pop rax")
    }

    it should "be able to format a mov instruction" in {
        val writer = new StringWriter()
        formatInstr(A_Mov(A_Reg(A_RegName.RetReg), A_Imm(5), INT_SIZE))(using writer)
        writer.toString should be ("mov eax, 5")
    }

    it should "be able to format a lea instruction" in {
        val writer = new StringWriter()
        formatInstr(A_Lea(
            A_Reg(A_RegName.RetReg), 
            A_MemOffset(A_Reg(A_RegName.RetReg), A_OffsetImm(5)))
        )(using writer)
        writer.toString should be ("lea rax, [rax + 5]")
    }
}

class format_function_ops_test extends AnyFlatSpec {
    "formatInstr" should "be able to format a call instruction" in {
        val writer = new StringWriter()
        formatInstr(A_Call(A_InstrLabel("malloc@plt")))(using writer)
        writer.toString should be ("call malloc@plt")
    }

    it should "be able to format a return instruction" in {
        val writer = new StringWriter()
        formatInstr(A_Ret)(using writer)
        writer.toString should be ("ret")
    }
}

class format_cond_test extends AnyFlatSpec {
    "formatCond" should "be able to format conditions" in {
        formatCond(A_Cond.Eq) should be ("e")
        formatCond(A_Cond.NEq) should be ("ne")
        formatCond(A_Cond.Gt) should be ("g")
        formatCond(A_Cond.Lt) should be ("l")
        formatCond(A_Cond.GEq) should be ("ge")
        formatCond(A_Cond.LEq) should be ("le")
        formatCond(A_Cond.Overflow) should be ("o")
        formatCond(A_Cond.NOverflow) should be ("no")
        formatCond(A_Cond.Uncond) should be ("mp")
    }
}