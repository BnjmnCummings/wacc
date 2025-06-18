package wacc.formatting

import wacc.assemblyIR.*

/**
 * Formats an instruction operand to a string.
 * @param op the operand to format.
 * @param opSize the size of the corresponding operand.
 * @return The formatted string of the operand.
 */
def formatOperand(op: A_Operand, opSize: A_OperandSize): String = op match
    case A_Imm(n) => n.toString
    case A_RegDeref(mem) => formatRegDeref(op.asInstanceOf[A_RegDeref], opSize)
    case A_MemOffset(reg, offset) => formatMemOffset(op.asInstanceOf[A_MemOffset])
    case A_Reg(regName) => formatReg(op.asInstanceOf[A_Reg], opSize)

/**
 * Formats a register dereference to a string.
 * qword ptr '[reg + offset]'
 * @param op the [A_RegDeref] operand to format.
 * @param opSize the size of the corresponding operand.
 * @return The formatted string of the operand.
 */
def formatRegDeref(op: A_RegDeref, opSize: A_OperandSize): String = opSize match
    case A_OperandSize.A_8 => s"byte ptr ${formatMemOffset(op.mem)}"
    case A_OperandSize.A_16 => s"word ptr ${formatMemOffset(op.mem)}"
    case A_OperandSize.A_32 => s"dword ptr ${formatMemOffset(op.mem)}"
    case A_OperandSize.A_64 => s"qword ptr ${formatMemOffset(op.mem)}"

/**
 * Formats the inner part of a register dereference to a string.
 * '[reg + offset]'
 * @param op the operand to format.
 * @return The formatted string of the operand.
 */
def formatMemOffset(op: A_MemOffset): String = 
    s"[${formatReg(op.reg, PTR_SIZE)} ${formatOffset(op.offset)}]"

/**
 * Formats a register offset.
 * @param offset the offset to format.
 * @return The formatted string of the offset.
 */
def formatOffset(offset: A_Offset): String = offset match
    case A_OffsetImm(n) => {
        if (n >= 0) 
            s"+ $n"
        else
            s"- ${-n}"
    }
    case A_OffsetLbl(lbl) => s"+ ${lbl.name}"

/**
 * Formats a register operand to a string.
 * @param reg the register to format.
 * @param regSize the size of the corresponding register.
 * @return The formatted string of the register.
 */
def formatReg(reg: A_Reg, regSize: A_OperandSize): String = regSize match
    case A_OperandSize.A_8 => reg.regName match
        case A_RegName.RetReg => "al"
        case A_RegName.Arg1 => "dil"
        case A_RegName.Arg2 => "sil"
        case A_RegName.Arg3 => "dl"
        case A_RegName.Arg4 => "cl"
        case A_RegName.Arg5 => "r8b"
        case A_RegName.Arg6 => "r9b"
        case A_RegName.R7 => "r10b"
        case A_RegName.R8 => "r11b"
        case A_RegName.R9 => "bl"
        case A_RegName.R10 => "r12b"
        case A_RegName.R11 => "r13b"
        case A_RegName.R12 => "r14b"
        case A_RegName.R13 => "r15b"
        case A_RegName.StackPtr => "spl"
        case A_RegName.BasePtr => "bpl"
        case A_RegName.InstrPtr => 
            throw UnsupportedOperationException("ILL TELL YOU WHAT LAD THERES NO SUCH THING AS AN 8 BIT INSTRUCTION POINTEH") 
    
    case A_OperandSize.A_16 => reg.regName match
        case A_RegName.RetReg => "ax"
        case A_RegName.Arg1 => "di"
        case A_RegName.Arg2 => "si"
        case A_RegName.Arg3 => "dx"
        case A_RegName.Arg4 => "cx"
        case A_RegName.Arg5 => "r8w"
        case A_RegName.Arg6 => "r9w"
        case A_RegName.R7 => "r10w"
        case A_RegName.R8 => "r11w"
        case A_RegName.R9 => "bx"
        case A_RegName.R10 => "r12w"
        case A_RegName.R11 => "r13w"
        case A_RegName.R12 => "r14w"
        case A_RegName.R13 => "r15w"
        case A_RegName.StackPtr => "sp"
        case A_RegName.BasePtr => "bp"
        case A_RegName.InstrPtr => "ip"

    case A_OperandSize.A_32 => reg.regName match
        case A_RegName.RetReg => "eax"
        case A_RegName.Arg1 => "edi"
        case A_RegName.Arg2 => "esi"
        case A_RegName.Arg3 => "edx"
        case A_RegName.Arg4 => "ecx"
        case A_RegName.Arg5 => "r8d"
        case A_RegName.Arg6 => "r9d"
        case A_RegName.R7 => "r10d"
        case A_RegName.R8 => "r11d"
        case A_RegName.R9 => "ebx"
        case A_RegName.R10 => "r12d"
        case A_RegName.R11 => "r13d"
        case A_RegName.R12 => "r14d"
        case A_RegName.R13 => "r15d"
        case A_RegName.StackPtr => "esp"
        case A_RegName.BasePtr => "ebp"
        case A_RegName.InstrPtr => "eip"


    case A_OperandSize.A_64 => reg.regName match
        case A_RegName.RetReg => "rax"
        case A_RegName.Arg1 => "rdi"
        case A_RegName.Arg2 => "rsi"
        case A_RegName.Arg3 => "rdx"
        case A_RegName.Arg4 => "rcx"
        case A_RegName.Arg5 => "r8"
        case A_RegName.Arg6 => "r9"
        case A_RegName.R7 => "r10"
        case A_RegName.R8 => "r11"
        case A_RegName.R9 => "rbx"
        case A_RegName.R10 => "r12"
        case A_RegName.R11 => "r13"
        case A_RegName.R12 => "r14"
        case A_RegName.R13 => "r15"
        case A_RegName.StackPtr => "rsp"
        case A_RegName.BasePtr => "rbp"
        case A_RegName.InstrPtr => "rip"

