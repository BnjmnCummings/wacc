package wacc.codeGen

import wacc.assemblyIR.*
import scala.collection.mutable.ListBuffer

// stack aligning for 16 bytes
inline def STACK_ALIGN_VAL = -16
val OVERFLOW_LBL_STR_NAME = ".L._errOverflow_str"
val PRINTLN_LBL_STR_NAME = ".L._println_str"
val ERR_EXIT_CODE = -1

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
    program += A_Push(A_Reg(PTR_SIZE, A_RegName.BasePtr))
    program += A_MovTo(A_Reg(PTR_SIZE, A_RegName.BasePtr), A_Reg(PTR_SIZE, A_RegName.StackPtr))
    program += A_And(A_Reg(PTR_SIZE, A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Call(A_ExternalLabel("exit"))
    program += A_MovTo(A_Reg(PTR_SIZE, A_RegName.StackPtr), A_Reg(PTR_SIZE, A_RegName.BasePtr))
    program += A_Pop(A_Reg(PTR_SIZE, A_RegName.BasePtr))
    program += A_Ret

    A_Func(A_InstrLabel("_exit"), program.toList)
}

inline def defaultOverflow: A_Func = {
    val program: ListBuffer[A_Instr] = ListBuffer()
    program += A_And(A_Reg(PTR_SIZE, A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Lea(A_Reg(PTR_SIZE, A_RegName.R1), A_MemOffset(PTR_SIZE, A_Reg(PTR_SIZE, A_RegName.InstrPtr), A_OffsetLbl(A_DataLabel(OVERFLOW_LBL_STR_NAME))))
    // TODO: CHECK THE OVERFLOW_LBL_STR_NAME constant
    program += A_Call(A_InstrLabel("_prints"))
    program += A_MovTo(A_Reg(EXIT_CODE_SIZE, A_RegName.R1), A_Imm(ERR_EXIT_CODE))
    program += A_Call(A_ExternalLabel("exit"))

    A_Func(A_InstrLabel("_errOverflow"), program.toList)
}

/*
push rbp
mov rbp, rsp
# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
and rsp, -16
lea rdi, [rip + .L._println_str0]
call puts@plt
mov rdi, 0
call fflush@plt
mov rsp, rbp
pop rbp
ret
*/

inline def defaultPrintln: A_Func = {
    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_Push(A_Reg(PTR_SIZE, A_RegName.BasePtr))
    program += A_MovTo(A_Reg(PTR_SIZE, A_RegName.BasePtr), A_Reg(PTR_SIZE, A_RegName.StackPtr))
    program += A_And(A_Reg(PTR_SIZE, A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Lea(A_Reg(PTR_SIZE, A_RegName.R1), A_MemOffset(PTR_SIZE, A_Reg(PTR_SIZE, A_RegName.InstrPtr), A_OffsetLbl(A_DataLabel(""))))
    program += A_Call(A_ExternalLabel("puts"))
    program += A_MovTo(A_Reg(PTR_SIZE, A_RegName.R1), A_Imm(ZERO_IMM))
    program += A_Call(A_ExternalLabel("fflush"))
    program += A_MovTo(A_Reg(PTR_SIZE, A_RegName.StackPtr), A_Reg(PTR_SIZE, A_RegName.BasePtr))
    program += A_Pop(A_Reg(PTR_SIZE, A_RegName.BasePtr))
    program += A_Ret

    A_Func(A_InstrLabel("_println"), program.toList)
}