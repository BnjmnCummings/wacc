package wacc.assemblyIR

case class A_Prog(data: List[A_StoredStr], funcs: List[A_Func])

sealed trait A_Proc

case class A_Func(lbl: A_InstrLabel, instrs: List[A_Instr]) extends A_Proc

case class A_DataFunc(lbl: A_InstrLabel, instrs: List[A_Instr], data: A_StoredStr) extends A_Proc

case class A_StoredStr(lbl: A_DataLabel, str: String)

sealed trait A_Instr

sealed trait A_ArithmeticOp extends A_Instr
sealed trait A_BitwiseOp extends A_Instr

case class A_Add(opD: A_Reg, opS: A_Operand, opSize: A_OperandSize) extends A_ArithmeticOp 
case class A_Sub(opD: A_Reg, opS: A_Operand, opSize: A_OperandSize) extends A_ArithmeticOp
case class A_Div(opD: A_Reg, opS: A_Operand, opSize: A_OperandSize) extends A_ArithmeticOp
case class A_IMul(opD: A_Reg, opS: A_Operand, opSize: A_OperandSize) extends A_ArithmeticOp
case class A_IDiv(op: A_Reg, opSize: A_OperandSize) extends A_ArithmeticOp

case class A_And(opD: A_Reg, opS: A_Operand, opSize: A_OperandSize) extends A_BitwiseOp 
case class A_Or(opD: A_Reg, opS: A_Operand, opSize: A_OperandSize) extends A_BitwiseOp 
case class A_Xor(opD: A_Reg, opS: A_Operand, opSize: A_OperandSize) extends A_BitwiseOp 

case class A_Cmp(op1: A_Reg, op2: A_Operand, opSize: A_OperandSize) extends A_Instr
case class A_Jmp(label: A_InstrLabel, condition: A_Cond) extends A_Instr
case class A_LabelStart(label: A_InstrLabel) extends A_Instr

case class A_Push(op: A_Reg) extends A_Instr
case class A_Pop(op: A_Reg) extends A_Instr
case class A_MovTo(opD: A_Reg, opS: A_Operand) extends A_Instr
case class A_MovFrom(opD: A_Operand, opS: A_Reg) extends A_Instr
case class A_MovDeref(opD: A_RegDeref, opS: A_Operand) extends A_Instr
case class A_MovFromDeref(opD: A_Operand, opS: A_RegDeref) extends A_Instr
case class A_Movzx(opD: A_Reg, opS: A_Reg) extends A_Instr
case class A_Lea(opD: A_Reg, opS: A_MemOffset) extends A_Instr

case class A_Set(op: A_Reg, condition: A_Cond) extends A_Instr

// clib is accessed using call
case class A_Call(label: A_Label) extends A_Instr

// Sign extend EAX into EAX:EDX
case object A_CDQ extends A_Instr // TODO @Ben @Zakk Implement string version of this

sealed trait A_Label:
    val name: String

case class A_DataLabel(val name: String) extends A_Label 
case class A_InstrLabel(val name: String) extends A_Label 

case class A_ExternalLabel(val name: String) extends A_Label

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
    case Uncond
}


