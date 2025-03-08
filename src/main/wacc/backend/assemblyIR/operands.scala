package wacc.assemblyIR

/**
  * Some cheeky macros to make the code more readable
  */
inline def TEMP_REG = A_Reg(A_RegName.R11)
inline def NO_OFFSET = A_OffsetImm(ZERO_IMM)
inline def INT_SIZE: A_OperandSize = A_OperandSize.A_32
inline def CHAR_SIZE: A_OperandSize = A_OperandSize.A_8
inline def BOOL_SIZE: A_OperandSize = A_OperandSize.A_8
inline def PTR_SIZE: A_OperandSize = A_OperandSize.A_64
inline def EXIT_CODE_SIZE = A_OperandSize.A_8
inline def TRUE = 1
inline def FALSE = 0
inline def ZERO_IMM = 0
inline def CHR_MASK = -128
inline def PAIR_SIZE_BYTES = 16
inline def PAIR_OFFSET_SIZE = 8
inline def MAIN_FUNC_NAME = "main"

/**
  * A_Operand, a sealed trait representing the different types of operands
  * @param n an immediate value of the operand
  * @param mem the memory address to be dereferenced
  * @param reg the register to be offset from
  * @param regName the name of the register to be used as an operand
  */
sealed trait A_Operand
case class A_Imm(n: BigInt) extends A_Operand
case class A_RegDeref(mem: A_MemOffset) extends A_Operand
case class A_MemOffset(reg: A_Reg, offset: A_Offset) extends A_Operand
case class A_Reg(regName: A_RegName) extends A_Operand

/**
  * A_Offset, a sealed trait representing the different types of memory address offsets
  * @param n the immediate value of the offset
  * @param lbl the label representing the offset value
  */
sealed trait A_Offset
case class A_OffsetImm(n: BigInt) extends A_Offset
case class A_OffsetLbl(lbl: A_DataLabel) extends A_Offset

/**
  * A_RegName, an enum representing the names of the registers in the x86-64 architecture
  * @see https://en.wikipedia.org/wiki/X86_calling_conventions
  * caller save: if you need to preserve the values of these before a function call, push and pop them on the stack
  * callee save: if you need to use this within a function call, that function must push and pop them on the stack
  */
enum A_RegName {
    case RetReg   // rax - caller
    case Arg1     // rdi - caller, 1st arg
    case Arg2     // rsi - caller, 2nd arg
    case Arg3     // rdx - caller, 3rd arg
    case Arg4     // rcx - caller, 4th arg
    case Arg5     // r8  - caller, 5th arg
    case Arg6     // r9  - caller, 6th arg
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

/**
  * A_OperandSize, an enum representing the sizes of the operands in the x86-64 architecture
  */
enum A_OperandSize {
    case A_8
    case A_16
    case A_32
    case A_64
}