package wacc.formatting

import wacc.assemblyIR.*

def formatInstr(instr: A_Instr): String = instr match
    case A_Add(opD, opS, _) => s"add ${formatReg(opD)}, ${formatOperand(opS)}"
    case A_Sub(opD, opS, _) => s"sub ${formatReg(opD)}, ${formatOperand(opS)}"
    case A_Mul(opD, opS, _) => s"mul ${formatReg(opD)}, ${formatOperand(opS)}"
    case A_Div(opD, opS, _) => s"div ${formatReg(opD)}, ${formatOperand(opS)}"
    case A_IMul(opD, op1, op2, _) => s"imul ${formatReg(opD)}, ${formatOperand(op1)}, ${formatOperand(op2)}"
    case A_IDiv(opD, _) => s"idiv ${formatReg(opD)}"

    case A_Cmp(op1, op2, _) => s"cmp ${formatReg(op1)}, ${formatOperand(op2)}"
    case A_Jmp(label, cond) => s"j${formatCond(cond)} ${label.name}"
    case A_Set(op, cond) => s"set${formatCond(cond)} ${{formatOperand(op)}}"
    case A_LabelStart(label) => s"${label.name}:"

    case A_And(opD, opS, _) => s"and ${formatReg(opD)}, ${formatOperand(opS)}"
    case A_Or(opD, opS, _) => s"or ${formatReg(opD)}, ${formatOperand(opS)}"
    case A_Xor(opD, opS, _) => s"xor ${formatReg(opD)}, ${formatOperand(opS)}"

    case A_Push(op) => s"push ${formatReg(op)}"
    case A_Pop(op) => s"pop ${formatReg(op)}"
    case A_MovTo(opD, opS) => s"mov ${formatReg(opD)}, ${formatOperand(opS)}"
    case A_MovFrom(opD, opS) => s"mov ${formatOperand(opD)}, ${formatReg(opS)}"
    case A_Movzx(opD, opS) => s"movzx ${formatReg(opD)}, ${formatReg(opS)}"
    case A_Lea(opD, opS) => s"lea ${formatReg(opD)}, ${formatMemOffset(opS)}"
    
    case A_Call(label) => s"call ${label.name}"
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
