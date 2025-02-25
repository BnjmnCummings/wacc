package wacc.codeGen

import wacc.assemblyIR.*
import scala.collection.mutable.ListBuffer

// stack aligning for 16 bytes
inline def STACK_ALIGN_VAL = -16

/* 
_exit:
    push rbp
    mov rbp, rsp
    and rsp, -16
    call exit@plt
    mov rsp, rbp
    pop rbp
    ret
*/

inline def defaultExit: A_Func = {
    val program: ListBuffer[A_Instr] = ListBuffer()
    program += A_Push(A_Reg(A_OperandSize.A_64, A_RegName.BasePtr))
    program += A_Mov(A_Reg(A_OperandSize.A_64, A_RegName.BasePtr), A_Reg(A_OperandSize.A_64, A_RegName.StackPtr))
    program += A_And(A_Reg(A_OperandSize.A_64, A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), A_OperandSize.A_64)
    program += A_Call(A_ExternalLabel("exit"))
    program += A_Mov(A_Reg(A_OperandSize.A_64, A_RegName.StackPtr), A_Reg(A_OperandSize.A_64, A_RegName.BasePtr))
    program += A_Pop(A_Reg(A_OperandSize.A_64, A_RegName.BasePtr))
    program += A_Ret

    A_Func(A_InstrLabel("_exit"), program.toList)
}