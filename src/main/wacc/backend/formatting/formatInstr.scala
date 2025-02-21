package wacc.formatting

import wacc.assemblyIR._

def formatInstr(instr: A_Instr): String = instr match
    case A_Add(opD, opS, _) => s"add ${formatReg(opD)}, ${formatOperand(opS)}"
    case A_Sub(opD, opS, _) => s"sub ${formatReg(opD)}, ${formatOperand(opS)}"
    case A_Mul(opD, opS, _) => s"mul ${formatReg(opD)}, ${formatOperand(opS)}"
    case A_Div(opD, opS, _) => s"div ${formatReg(opD)}, ${formatOperand(opS)}"
    
    case A_Cmp(op1, op2, _) => s"cmp ${formatReg(op1)}, ${formatOperand(op2)}"
    case A_Jmp(label, cond) => s"j${formatCond(cond)} ${label.name}"
    case A_LabelStart(label) => s"${label.name}:"

    case A_Push(op) => s"push ${formatReg(op)}"
    case A_Pop(op) => s"pop ${formatReg(op)}"
    case A_Mov(opD, opS) => s"mov ${formatReg(opD)}, ${formatOperand(opS)}"
    case A_Lea(opD, opS) => s"lea ${formatReg(opD)}, ${formatMemOffset(opS)}"
    
    case A_Call(label) => s"call ${label.name}"
    case A_Ret => "ret"

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
