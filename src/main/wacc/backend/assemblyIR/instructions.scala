package wacc.assemblyIR

case class A_Prog(instrs: List[A_Instr])

sealed trait A_Instr

sealed trait A_ArithmeticOp extends A_Instr
sealed trait A_BooleanOp extends A_Instr

// TODO: CAN WE ALWAYS USE FANCY ADDRESSING MODES

case class A_Add(opS: A_NumOperand, opD: A_RegDeref, opSize: A_OperandSize) extends A_ArithmeticOp 
case class A_Sub(opS: A_NumOperand, opD: A_RegDeref, opSize: A_OperandSize) extends A_ArithmeticOp
case class A_Mul(opS: A_NumOperand, opD: A_RegDeref, opSize: A_OperandSize) extends A_ArithmeticOp
case class A_Div(opS: A_NumOperand, opD: A_RegDeref, opSize: A_OperandSize) extends A_ArithmeticOp

case class A_Cmp(op1: A_CmpOperand, op2: A_CmpOperand, opSize: A_OperandSize) extends A_Instr
case class A_Jmp(label: A_Lbl, condition: A_JmpCond) extends A_Instr

case class A_And(opS: A_BoolOperand, opD: A_RegDeref, opSize: A_OperandSize) extends A_Instr
case class A_Or(opS: A_BoolOperand, opD: A_RegDeref, opSize: A_OperandSize) extends A_Instr

case class A_Push(op: A_Reg)
case class A_Pop(op: A_Reg)
case class A_Mov(opD: A_RegDerefSimple, opS: A_RegDeref)
case class A_Lea(opD: A_RegDerefSimple, opS: A_RegDeref) 


// TODO: Add Clib/IO support

case class A_StoredStr(name: String, str: String) extends A_Instr

case class A_Lbl(name: String)

case object A_Ret extends A_Instr

enum A_JmpCond {
    case Eq
    case NEq
    case Gt
    case Lt
    case GEq
    case LEq
    case Overflow
    case NOverflow
}


