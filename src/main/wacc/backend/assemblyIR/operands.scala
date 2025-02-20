package wacc.assemblyIR

sealed trait A_Operand

sealed trait A_RegDeref

case class A_RegDerefSimple(opSize: A_OperandSize, reg: A_Reg) extends A_RegDeref
case class A_RegDerefPlusImm(opSize: A_OperandSize, reg: A_Reg, imm: BigInt) extends A_RegDeref
case class A_RegDerefPlusReg(opSize: A_OperandSize, reg: A_Reg, reg2: A_Reg) extends A_RegDeref
case class A_RegDerefPlusScaledReg(opSize: A_OperandSize, reg: A_Reg, reg2: A_Reg, imm: BigInt) extends A_RegDeref

case class A_Reg(regSize: A_OperandSize, regName: A_RegName)

/* 
caller save - if you need to preserve the values of these before a function call, push and pop them on the stack
callee save - if you need to use this within a function call, that function must push and pop them on the stack
*/

enum A_RegName {
    case RetReg   // rax - caller
    case R1       // rdi - caller, 1st arg
    case R2       // rsi - caller, 2nd arg
    case R3       // rdx - caller, 3rd arg
    case R4       // rcx - caller, 4th arg
    case R5       // r8 - caller, 5th arg
    case R6       // r9 - caller, 6th arg
    case R7       // r10 - caller
    case R8       // r11 - caller
    case R9       // rbx - callee
    case R10      // r12 - callee
    case R11      // r13 - callee
    case R12      // r14 - callee
    case R13      // r15 - callee
    case StackPtr // rsp - callee
    case BasePtr  // rbp - callee
}

enum A_OperandSize {
    case A_8
    case A_16
    case A_32
    case A_64
}

case class A_Imm(n: BigInt) extends A_Operand