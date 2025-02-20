package wacc.assemblyIR

case class A_Prog(data: List[A_StoredStr], funcs: List[A_Func])

case class A_Func(lbl: A_Lbl, instrs: List[A_Instr])

case class A_StoredStr(lbl: A_Lbl, str: String)

sealed trait A_Instr

sealed trait A_ArithmeticOp extends A_Instr
sealed trait A_BitwiseOp extends A_Instr

case class A_Add(opD: A_RegDeref, opS: A_NumOperand, opSize: A_OperandSize) extends A_ArithmeticOp 
case class A_Sub(opD: A_RegDeref, opS: A_NumOperand, opSize: A_OperandSize) extends A_ArithmeticOp
case class A_Mul(opD: A_RegDeref, opS: A_NumOperand, opSize: A_OperandSize) extends A_ArithmeticOp
case class A_Div(opD: A_RegDeref, opS: A_NumOperand, opSize: A_OperandSize) extends A_ArithmeticOp

case class A_Cmp(op1: A_RegDeref, op2: A_Operand, opSize: A_OperandSize) extends A_Instr
case class A_Jmp(label: A_Lbl, condition: A_Cond) extends A_Instr

case class A_And(opD: A_RegDeref, opS: A_Operand, opSize: A_OperandSize) extends A_BitwiseOp
case class A_Or(opD: A_RegDeref, opS: A_Operand, opSize: A_OperandSize) extends A_BitwiseOp

case class A_Push(op: A_Reg) extends A_Instr
case class A_Pop(op: A_Reg) extends A_Instr
case class A_Mov(opD: A_RegDerefSimple, opS: A_Operand) extends A_Instr
case class A_Lea(opD: A_RegDerefSimple, opS: A_RegDeref) extends A_Instr

// clib is accessed using call
case class A_Call(label: A_Lbl) extends A_Instr

case class A_Lbl(name: String)

case object A_Ret extends A_Instr

enum A_Cond {
    case Eq
    case NEq
    case Gt
    case Lt
    case GEq
    case LEq
    case Overflow
    case NOverflow
}


