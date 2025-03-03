package wacc.formatting

import wacc.assemblyIR.*

def formatOperand(op: A_Operand, opSize: A_OperandSize): String = op match
    case A_Imm(n) => n.toString
    case A_RegDeref(mem) => formatRegDeref(op.asInstanceOf[A_RegDeref], opSize)
    case A_MemOffset(reg, offset) => formatMemOffset(op.asInstanceOf[A_MemOffset])
    case A_Reg(regName) => formatReg(op.asInstanceOf[A_Reg], opSize)

def formatRegDeref(op: A_RegDeref, opSize: A_OperandSize): String = opSize match
    case A_OperandSize.A_8 => s"byte ptr ${formatMemOffset(op.mem)}"
    case A_OperandSize.A_16 => s"word ptr ${formatMemOffset(op.mem)}"
    case A_OperandSize.A_32 => s"dword ptr ${formatMemOffset(op.mem)}"
    case A_OperandSize.A_64 => s"qword ptr ${formatMemOffset(op.mem)}"

def formatMemOffset(op: A_MemOffset): String = s"[${formatReg(op.reg, PTR_SIZE)} ${formatOffset(op.offset)}]"

def formatOffset(op: A_Offset): String = op match
    case A_OffsetImm(n) => {
        if (n >= 0) 
            s"+ $n"
        else
            s"- ${-n}"
    }
    case A_OffsetReg(reg) => s"+ ${formatReg(reg, PTR_SIZE)}"
    case A_OffsetLbl(lbl) => s"+ ${lbl.name}"

def formatReg(op: A_Reg, opSize: A_OperandSize): String = opSize match
    case A_OperandSize.A_8 => op.regName match
        case A_RegName.RetReg => "al"
        case A_RegName.R1 => "dil"
        case A_RegName.R2 => "sil"
        case A_RegName.R3 => "dl"
        case A_RegName.R4 => "cl"
        case A_RegName.R5 => "r8b"
        case A_RegName.R6 => "r9b"
        case A_RegName.R7 => "r10b"
        case A_RegName.R8 => "r11b"
        case A_RegName.R9 => "bl"
        case A_RegName.R10 => "r12b"
        case A_RegName.R11 => "r13b"
        case A_RegName.R12 => "r14b"
        case A_RegName.R13 => "r15b"
        case A_RegName.StackPtr => "spl"
        case A_RegName.BasePtr => "bpl"
    
    case A_OperandSize.A_16 => op.regName match
        case A_RegName.RetReg => "ax"
        case A_RegName.R1 => "di"
        case A_RegName.R2 => "si"
        case A_RegName.R3 => "dx"
        case A_RegName.R4 => "cx"
        case A_RegName.R5 => "r8w"
        case A_RegName.R6 => "r9w"
        case A_RegName.R7 => "r10w"
        case A_RegName.R8 => "r11w"
        case A_RegName.R9 => "bx"
        case A_RegName.R10 => "r12w"
        case A_RegName.R11 => "r13w"
        case A_RegName.R12 => "r14w"
        case A_RegName.R13 => "r15w"
        case A_RegName.StackPtr => "sp"
        case A_RegName.BasePtr => "bp"

    case A_OperandSize.A_32 => op.regName match
        case A_RegName.RetReg => "eax"
        case A_RegName.R1 => "edi"
        case A_RegName.R2 => "esi"
        case A_RegName.R3 => "edx"
        case A_RegName.R4 => "ecx"
        case A_RegName.R5 => "r8d"
        case A_RegName.R6 => "r9d"
        case A_RegName.R7 => "r10d"
        case A_RegName.R8 => "r11d"
        case A_RegName.R9 => "ebx"
        case A_RegName.R10 => "r12d"
        case A_RegName.R11 => "r13d"
        case A_RegName.R12 => "r14d"
        case A_RegName.R13 => "r15d"
        case A_RegName.StackPtr => "esp"
        case A_RegName.BasePtr => "ebp"

    case A_OperandSize.A_64 => op.regName match
        case A_RegName.RetReg => "rax"
        case A_RegName.R1 => "rdi"
        case A_RegName.R2 => "rsi"
        case A_RegName.R3 => "rdx"
        case A_RegName.R4 => "rcx"
        case A_RegName.R5 => "r8"
        case A_RegName.R6 => "r9"
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

