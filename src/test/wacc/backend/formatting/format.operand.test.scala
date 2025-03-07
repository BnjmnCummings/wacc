package wacc.formatting

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import wacc.formatting.{formatOperand, formatRegDeref, formatMemOffset, formatOffset, formatReg}
import wacc.assemblyIR.*  

class format_operand_test extends AnyFlatSpec {
    "formatOperand" should "be able to format an immediate operand" in {
        formatOperand(A_Imm(5), INT_SIZE) should be ("5")
    }

    it should "be able to format a negative immediate operand" in {
        formatOperand(A_Imm(-5), INT_SIZE) should be ("-5")
    }

    it should "be able to format a basic register dereference" in {
        formatOperand(A_RegDeref(A_MemOffset(A_Reg(A_RegName.RetReg), A_OffsetImm(5))), A_OperandSize.A_32) should be ("dword ptr [rax + 5]")
    }

    it should "be able to format a memory offset" in {
        formatOperand(A_MemOffset(A_Reg(A_RegName.RetReg), A_OffsetImm(5)), A_OperandSize.A_32) should be ("[rax + 5]")
    }

    it should "be able to format a register" in {
        formatOperand(A_Reg(A_RegName.RetReg), A_OperandSize.A_32) should be ("eax")
    }
}

class format_reg_deref_test extends AnyFlatSpec {
    "formatRegDeref" should "be able to format an 8 bit register dereference" in {
        formatRegDeref(A_RegDeref( 
            A_MemOffset(A_Reg(A_RegName.RetReg), 
            A_OffsetImm(5))),
            A_OperandSize.A_8
            ) should be ("byte ptr [rax + 5]")
    }

    it should "be able to format a 16 bit register dereference" in {
        formatRegDeref(A_RegDeref( 
            A_MemOffset(A_Reg(A_RegName.RetReg), 
            A_OffsetImm(5))),
            A_OperandSize.A_16
            ) should be ("word ptr [rax + 5]")
    }

    it should "be able to format a 32 bit register dereference" in {
        formatRegDeref(A_RegDeref( 
            A_MemOffset(A_Reg(A_RegName.RetReg), 
            A_OffsetImm(5))),
            A_OperandSize.A_32
            ) should be ("dword ptr [rax + 5]")
    }

    it should "be able to format a 64 bit register dereference" in {
        formatRegDeref(A_RegDeref( 
            A_MemOffset(A_Reg(A_RegName.RetReg), 
            A_OffsetImm(5))),
            A_OperandSize.A_64
            ) should be ("qword ptr [rax + 5]")
    }
}

class format_mem_offset_test extends AnyFlatSpec {
    "formatMemOffset" should "be able to format a positive immediate offset" in {
        formatMemOffset(A_MemOffset(
            A_Reg(A_RegName.RetReg), 
            A_OffsetImm(5))
            ) should be ("[rax + 5]")
    }

    it should "be able to format a negative immediate offset" in {
        formatMemOffset(A_MemOffset(
            A_Reg(A_RegName.RetReg), 
            A_OffsetImm(-5))
            ) should be ("[rax - 5]")
    }

    it should "be able to format a register offset" in {
        formatMemOffset(A_MemOffset(
            A_Reg(A_RegName.RetReg), 
            A_OffsetReg(A_Reg(A_RegName.Arg1)))
            ) should be ("[rax + rdi]")
    }
}

class format_offset_test extends AnyFlatSpec {
    "formatOffset" should "be able to format a positive immediate offset" in {
        formatOffset(A_OffsetImm(5)) should be ("+ 5")
    }

    it should "be able to format a negative immediate offset" in {
        formatOffset(A_OffsetImm(-5)) should be ("- 5")
    }

    it should "be able to format a register offset" in {
        formatOffset(A_OffsetReg(A_Reg(A_RegName.Arg1))) should be ("+ rdi")
    }
}

class format_reg_test extends AnyFlatSpec {
    "formatReg" should "be able to format 64 bit registers" in {
        formatReg(A_Reg(A_RegName.RetReg), A_OperandSize.A_64) should be ("rax")
        formatReg(A_Reg(A_RegName.Arg1), A_OperandSize.A_64) should be ("rdi")
        formatReg(A_Reg(A_RegName.Arg2), A_OperandSize.A_64) should be ("rsi")
        formatReg(A_Reg(A_RegName.Arg3), A_OperandSize.A_64) should be ("rdx")
        formatReg(A_Reg(A_RegName.Arg4), A_OperandSize.A_64) should be ("rcx")
        formatReg(A_Reg(A_RegName.Arg5), A_OperandSize.A_64) should be ("r8")
        formatReg(A_Reg(A_RegName.Arg6), A_OperandSize.A_64) should be ("r9")
        formatReg(A_Reg(A_RegName.R7), A_OperandSize.A_64) should be ("r10")
        formatReg(A_Reg(A_RegName.R8), A_OperandSize.A_64) should be ("r11")
        formatReg(A_Reg(A_RegName.R9), A_OperandSize.A_64) should be ("rbx")
        formatReg(A_Reg(A_RegName.R10), A_OperandSize.A_64) should be ("r12")
        formatReg(A_Reg(A_RegName.R11), A_OperandSize.A_64) should be ("r13")
        formatReg(A_Reg(A_RegName.R12), A_OperandSize.A_64) should be ("r14")
        formatReg(A_Reg(A_RegName.R13), A_OperandSize.A_64) should be ("r15")
        formatReg(A_Reg(A_RegName.StackPtr), A_OperandSize.A_64) should be ("rsp")
        formatReg(A_Reg(A_RegName.BasePtr), A_OperandSize.A_64) should be ("rbp")
    }

    it should "be able to format 32 bit registers" in {
        formatReg(A_Reg(A_RegName.RetReg), A_OperandSize.A_32) should be ("eax")
        formatReg(A_Reg(A_RegName.Arg1), A_OperandSize.A_32) should be ("edi")
        formatReg(A_Reg(A_RegName.Arg2), A_OperandSize.A_32) should be ("esi")
        formatReg(A_Reg(A_RegName.Arg3), A_OperandSize.A_32) should be ("edx")
        formatReg(A_Reg(A_RegName.Arg4), A_OperandSize.A_32) should be ("ecx")
        formatReg(A_Reg(A_RegName.Arg5), A_OperandSize.A_32) should be ("r8d")
        formatReg(A_Reg(A_RegName.Arg6), A_OperandSize.A_32) should be ("r9d")
        formatReg(A_Reg(A_RegName.R7), A_OperandSize.A_32) should be ("r10d")
        formatReg(A_Reg(A_RegName.R8), A_OperandSize.A_32) should be ("r11d")
        formatReg(A_Reg(A_RegName.R9), A_OperandSize.A_32) should be ("ebx")
        formatReg(A_Reg(A_RegName.R10), A_OperandSize.A_32) should be ("r12d")
        formatReg(A_Reg(A_RegName.R11), A_OperandSize.A_32) should be ("r13d")
        formatReg(A_Reg(A_RegName.R12), A_OperandSize.A_32) should be ("r14d")
        formatReg(A_Reg(A_RegName.R13), A_OperandSize.A_32) should be ("r15d")
        formatReg(A_Reg(A_RegName.StackPtr), A_OperandSize.A_32) should be ("esp")
        formatReg(A_Reg(A_RegName.BasePtr), A_OperandSize.A_32) should be ("ebp")
    }

    it should "be able to format 16 bit registers" in {
        formatReg(A_Reg(A_RegName.RetReg), A_OperandSize.A_16) should be ("ax")
        formatReg(A_Reg(A_RegName.Arg1), A_OperandSize.A_16) should be ("di")
        formatReg(A_Reg(A_RegName.Arg2), A_OperandSize.A_16) should be ("si")
        formatReg(A_Reg(A_RegName.Arg3), A_OperandSize.A_16) should be ("dx")
        formatReg(A_Reg(A_RegName.Arg4), A_OperandSize.A_16) should be ("cx")
        formatReg(A_Reg(A_RegName.Arg5), A_OperandSize.A_16) should be ("r8w")
        formatReg(A_Reg(A_RegName.Arg6), A_OperandSize.A_16) should be ("r9w")
        formatReg(A_Reg(A_RegName.R7), A_OperandSize.A_16) should be ("r10w")
        formatReg(A_Reg(A_RegName.R8), A_OperandSize.A_16) should be ("r11w")
        formatReg(A_Reg(A_RegName.R9), A_OperandSize.A_16) should be ("bx")
        formatReg(A_Reg(A_RegName.R10), A_OperandSize.A_16) should be ("r12w")
        formatReg(A_Reg(A_RegName.R11), A_OperandSize.A_16) should be ("r13w")
        formatReg(A_Reg(A_RegName.R12), A_OperandSize.A_16) should be ("r14w")
        formatReg(A_Reg(A_RegName.R13), A_OperandSize.A_16) should be ("r15w")
        formatReg(A_Reg(A_RegName.StackPtr), A_OperandSize.A_16) should be ("sp")
        formatReg(A_Reg(A_RegName.BasePtr), A_OperandSize.A_16) should be ("bp")
    }

    it should "be able to format 8 bit registers" in {
        formatReg(A_Reg(A_RegName.RetReg), A_OperandSize.A_8) should be ("al")
        formatReg(A_Reg(A_RegName.Arg1), A_OperandSize.A_8) should be ("dil")
        formatReg(A_Reg(A_RegName.Arg2), A_OperandSize.A_8) should be ("sil")
        formatReg(A_Reg(A_RegName.Arg3), A_OperandSize.A_8) should be ("dl")
        formatReg(A_Reg(A_RegName.Arg4), A_OperandSize.A_8) should be ("cl")
        formatReg(A_Reg(A_RegName.Arg5), A_OperandSize.A_8) should be ("r8b")
        formatReg(A_Reg(A_RegName.Arg6), A_OperandSize.A_8) should be ("r9b")
        formatReg(A_Reg(A_RegName.R7), A_OperandSize.A_8) should be ("r10b")
        formatReg(A_Reg(A_RegName.R8), A_OperandSize.A_8) should be ("r11b")
        formatReg(A_Reg(A_RegName.R9), A_OperandSize.A_8) should be ("bl")
        formatReg(A_Reg(A_RegName.R10), A_OperandSize.A_8) should be ("r12b")
        formatReg(A_Reg(A_RegName.R11), A_OperandSize.A_8) should be ("r13b")
        formatReg(A_Reg(A_RegName.R12), A_OperandSize.A_8) should be ("r14b")
        formatReg(A_Reg(A_RegName.R13), A_OperandSize.A_8) should be ("r15b")
        formatReg(A_Reg(A_RegName.StackPtr), A_OperandSize.A_8) should be ("spl")
        formatReg(A_Reg(A_RegName.BasePtr), A_OperandSize.A_8) should be ("bpl")
    }
}