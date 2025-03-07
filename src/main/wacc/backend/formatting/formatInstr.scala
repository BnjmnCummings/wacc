package wacc.formatting

import wacc.assemblyIR.*
import java.io.Writer

def formatInstr(instr: A_Instr)(using writer: Writer): Unit = instr match
    // case A_Add(opD, opS, opSize) => s"add ${formatReg(opD, opSize)}, ${formatOperand(opS, opSize)}"
    case A_Add(opD, opS, opSize) => formatBinaryOp(opD, opS, opSize, opSize, "add")
    case A_Sub(opD, opS, opSize) => formatBinaryOp(opD, opS, opSize, opSize, "sub")
    case A_Div(opD, opS, opSize) => formatBinaryOp(opD, opS, opSize, opSize, "div")
    case A_IMul(opD, opS, opSize) => formatBinaryOp(opD, opS, opSize, opSize, "imul")
    case A_IDiv(opD, opSize) => formatUnaryOp(opD, opSize, "idiv")

    // case A_Cmp(op1, op2, opSize) => s"cmp ${formatReg(op1, opSize)}, ${formatOperand(op2, opSize)}"
    case A_Cmp(op1, op2, opSize) => formatBinaryOp(op1, op2, opSize, opSize, "cmp")
    case A_Jmp(label, cond) => writer.write(s"j${formatCond(cond)} ${label.name}")
    case A_Set(op, cond) => writer.write(s"set${formatCond(cond)} ${{formatOperand(op, BOOL_SIZE)}}")
    case A_LabelStart(label) => writer.write(s"${label.name}:")

    case A_And(opD, opS, opSize) => formatBinaryOp(opD, opS, opSize, opSize, "and")
    case A_Xor(opD, opS, opSize) => writer.write(s"xor ${formatReg(opD, opSize)}, ${formatOperand(opS, opSize)}")

    case A_Push(op) => formatUnaryOp(op, PTR_SIZE, "push")
    case A_Pop(op) => formatUnaryOp(op, PTR_SIZE, "pop")
    case A_Mov(opD, opS, opSize) => formatBinaryOp(opD, opS, opSize, opSize, "mov")
    case A_Mov(opD, opS, opSize) => formatBinaryOp(opD, opS, opSize, opSize, "mov")
    case A_Movzx(opD, opS, opSize1, opSize2) => formatBinaryOp(opD, opS, opSize1, opSize2, "movzx")
    case A_Lea(opD, opS) => formatBinaryOp(opD, opS, PTR_SIZE, PTR_SIZE, "lea")
    
    case A_Call(label) => label match
        case A_InstrLabel(name) => writer.write(s"call $name")
        case A_ExternalLabel(name) => writer.write(s"call $name@plt")
        case A_DataLabel(_) => throw new IllegalArgumentException("Cannot call a data label")
        case A_DefaultLabel(name) => writer.write(s"call $name")
    case A_Ret => writer.write("ret")

    case A_CDQ => writer.write("cdq")

inline def formatBinaryOp(opD: A_Operand, opS: A_Operand, opSizeD: A_OperandSize, opSizeS: A_OperandSize, op: String)(using writer: Writer): Unit = 
    writer.write(s"$op ${formatOperand(opD, opSizeD)}, ${formatOperand(opS, opSizeS)}")

inline def formatUnaryOp(op: A_Operand, opSize: A_OperandSize, opName: String)(using writer: Writer): Unit = 
    writer.write(s"$opName ${formatOperand(op, opSize)}")

def formatCond(cond: A_Cond): String = cond match
    case A_Cond.Eq => "e"
    case A_Cond.NEq => "ne"
    case A_Cond.Gt => "g"
    case A_Cond.Lt => "l"
    case A_Cond.GEq => "ge"
    case A_Cond.LEq => "le"
    case A_Cond.Overflow => "o"
    case A_Cond.NOverflow => "no"
    case A_Cond.Uncond => "mp"
