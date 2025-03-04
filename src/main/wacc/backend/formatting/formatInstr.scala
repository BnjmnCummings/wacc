package wacc.formatting

import wacc.assemblyIR.*

def formatInstr(instr: A_Instr): String = instr match
    case A_Add(opD, opS, opSize) => s"add ${formatReg(opD, opSize)}, ${formatOperand(opS, opSize)}"
    case A_Sub(opD, opS, opSize) => s"sub ${formatReg(opD, opSize)}, ${formatOperand(opS, opSize)}"
    case A_Div(opD, opS, opSize) => s"div ${formatReg(opD, opSize)}, ${formatOperand(opS, opSize)}"
    case A_IMul(opD, opS, opSize) => s"imul ${formatReg(opD, opSize)}, ${formatOperand(opS, opSize)}"
    case A_IDiv(opD, opSize) => s"idiv ${formatReg(opD, opSize)}"

    case A_Cmp(op1, op2, opSize) => s"cmp ${formatReg(op1, opSize)}, ${formatOperand(op2, opSize)}"
    case A_Jmp(label, cond) => s"j${formatCond(cond)} ${label.name}"
    case A_Set(op, cond) => s"set${formatCond(cond)} ${{formatOperand(op, BOOL_SIZE)}}"
    case A_LabelStart(label) => s"${label.name}:"

    case A_And(opD, opS, opSize) => s"and ${formatReg(opD, opSize)}, ${formatOperand(opS, opSize)}"
    case A_Or(opD, opS, opSize) => s"or ${formatReg(opD, opSize)}, ${formatOperand(opS, opSize)}"
    case A_Xor(opD, opS, opSize) => s"xor ${formatReg(opD, opSize)}, ${formatOperand(opS, opSize)}"

    case A_Push(op) => s"push ${formatReg(op, PTR_SIZE)}"
    case A_Pop(op) => s"pop ${formatReg(op, PTR_SIZE)}"
    case A_MovTo(opD, opS, opSize) => s"mov ${formatReg(opD, opSize)}, ${formatOperand(opS, opSize)}"
    case A_MovFrom(opD, opS, opSize) => s"mov ${formatOperand(opD, opSize)}, ${formatReg(opS, opSize)}"
    case A_Movzx(opD, opS, opSize1, opSize2) => s"movzx ${formatReg(opD, opSize1)}, ${formatReg(opS, opSize2)}"
    case A_MovDeref(opD, opS, opSize) => s"mov ${{formatRegDeref(opD, opSize)}}, ${formatOperand(opS, opSize)}"
    case A_MovFromDeref(opD, opS, opSize) => s"mov ${formatOperand(opD, opSize)}, ${{formatRegDeref(opS, opSize)}}"
    case A_Lea(opD, opS) => s"lea ${formatReg(opD, PTR_SIZE)}, ${formatMemOffset(opS)}"
    
    case A_Call(label) => label match
        case A_InstrLabel(name) => s"call $name"
        case A_ExternalLabel(name) => s"call $name@plt"
        case A_DataLabel(_) => throw new IllegalArgumentException("Cannot call a data label")
    case A_Ret => "ret"

    case A_CDQ => "cdq"

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
