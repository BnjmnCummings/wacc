package wacc.assemblyIR

sealed trait A_Operand

case class A_Imm(n: BigInt) extends A_Operand
case class A_RegDeref(mem: A_MemOffset) extends A_Operand
case class A_MemOffset(reg: A_Reg, offset: A_Offset) extends A_Operand
case class A_Reg(regName: A_RegName) extends A_Operand

sealed trait A_Offset
case class A_OffsetImm(n: BigInt) extends A_Offset
case class A_OffsetReg(reg: A_Reg) extends A_Offset
case class A_OffsetLbl(lbl: A_DataLabel) extends A_Offset

/* cheeky macro for 0 offset */
inline def noOffset: A_Offset = A_OffsetImm(0)

inline def INT_SIZE: A_OperandSize = A_OperandSize.A_32
inline def CHAR_SIZE: A_OperandSize = A_OperandSize.A_8
inline def BOOL_SIZE: A_OperandSize = A_OperandSize.A_8
inline def PTR_SIZE: A_OperandSize = A_OperandSize.A_64
inline def EXIT_CODE_SIZE = A_OperandSize.A_8

/**
  * caller save - if you need to preserve the values of these before a function call, push and pop them on the stack
  * callee save - if you need to use this within a function call, that function must push and pop them on the stack
  */
enum A_RegName {
    case RetReg   // rax - caller
    case Arg1       // rdi - caller, 1st arg
    case Arg2       // rsi - caller, 2nd arg
    case Arg3       // rdx - caller, 3rd arg
    case Arg4       // rcx - caller, 4th arg
    case Arg5       // r8 - caller, 5th arg
    case Arg6       // r9 - caller, 6th arg
    case R7       // r10 - caller
    case R8       // r11 - caller
    case R9       // rbx - callee
    case R10      // r12 - callee
    case R11      // r13 - callee
    case R12      // r14 - callee
    case R13      // r15 - callee
    case StackPtr // rsp - callee
    case BasePtr  // rbp - callee
    case InstrPtr // rip
}

enum A_OperandSize {
    case A_8
    case A_16
    case A_32
    case A_64
}