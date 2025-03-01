package wacc.codeGen

import wacc.assemblyIR.*
import scala.collection.mutable.ListBuffer

// stack aligning for 16 bytes
inline def STACK_ALIGN_VAL = -16
val ERR_EXIT_CODE = -1

val BYTE_SIZE = A_OperandSize.A_8

val OVERFLOW_LBL_STR_NAME = ".L._errOverflow_str"
val DIV_ZERO_LBL_STR_NAME = ".L._errDivZero_str"
val OUT_OF_BOUNDS_LBL_STR_NAME = ".L._errOutOfBounds_str"
val OUT_OF_MEMORY_LBL_STR_NAME = ".L._errOutOfMemory_str"
val PRINTLN_LBL_STR_NAME = ".L._println_str"
val PRINTI_LBL_STR_NAME = ".L._printi_int"
val PRINTC_LBL_STR_NAME = ".L._printc_str"
val PRINTP_LBL_STR_NAME = ".L._printp_str"
val PRINTB_TRUE_LBL_STR_NAME = ".L._printb_str_true"
val PRINTB_FALSE_LBL_STR_NAME = ".L._printb_str_false"
val PRINTB_LBL_STR_NAME = ".L._printb_str"
val PRINTS_LBL_STR_NAME = ".L._prints_str"
val ERR_BAD_CHAR_STR_NAME = ".L._errBadChar_str"

val F_FLUSH = "fflush"
val PUTS = "puts"
val EXIT = "exit"
val PRINTF = "printf"

val ERR_BAD_CHAR_LABEL = "_errBadChar"
val ERR_OVERFLOW_LABEL = "_errOverflow"
val ERR_OUT_OF_BOUNDS_LABEL = "_errOutOfBounds"
val ERR_OUT_OF_MEMORY_LABEL = "_errOutOfMemory"
val ERR_DIV_ZERO_LABEL = "_errDivZero"
val PRINTLN_LABEL = "_println"
val PRINTI_LABEL = "_printi"
val PRINTC_LABEL = "_printc"
val PRINTP_LABEL = "_printp"
val PRINTB_LABEL = "_printb"
val PRINTS_LABEL = "_prints"
val PRINTB_FALSE_LABEL = "_printb_false"
val PRINTB_TRUE_LABEL = "_printb_true"
val EXIT_LABEL = "_exit"
val ARR_LD1_LABEL = "_arrLoad1"
val ARR_LD4_LABEL = "_arrLoad4"
val ARR_LD8_LABEL = "_arrLoad8"
val MALLOC_LABEL = "_malloc"
val FREE_LABEL = "_free"
val FREE_PAIR_LABEL = "_freePair"

inline def defaultExit: A_Func = {
    val program: ListBuffer[A_Instr] = ListBuffer()
    program += A_Push(A_Reg(PTR_SIZE, A_RegName.BasePtr))
    program += A_MovTo(A_Reg(PTR_SIZE, A_RegName.BasePtr), A_Reg(PTR_SIZE, A_RegName.StackPtr))
    program += A_And(A_Reg(PTR_SIZE, A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Call(A_ExternalLabel("exit"))
    program += A_MovTo(A_Reg(PTR_SIZE, A_RegName.StackPtr), A_Reg(PTR_SIZE, A_RegName.BasePtr))
    program += A_Pop(A_Reg(PTR_SIZE, A_RegName.BasePtr))
    program += A_Ret

    A_Func(A_InstrLabel(EXIT_LABEL), program.toList)
}

inline def defaultOverflow: A_Func = {
    val program: ListBuffer[A_Instr] = ListBuffer()
    program += A_And(A_Reg(PTR_SIZE, A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Lea(A_Reg(PTR_SIZE, A_RegName.R1), A_MemOffset(PTR_SIZE, A_Reg(PTR_SIZE, A_RegName.InstrPtr), A_OffsetLbl(A_DataLabel(OVERFLOW_LBL_STR_NAME))))
    program += A_Call(A_InstrLabel(PRINTS_LABEL))
    program += A_MovTo(A_Reg(EXIT_CODE_SIZE, A_RegName.R1), A_Imm(ERR_EXIT_CODE))
    program += A_Call(A_ExternalLabel("exit"))

    A_Func(A_InstrLabel(ERR_OVERFLOW_LABEL), program.toList)
}

// When calling print:
    // edi holds the format string
    // esi holds the value to be printed

inline def defaultPrintln: A_Func = {
    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_Push(A_Reg(PTR_SIZE, A_RegName.BasePtr))
    program += A_MovTo(A_Reg(PTR_SIZE, A_RegName.BasePtr), A_Reg(PTR_SIZE, A_RegName.StackPtr))
    program += A_And(A_Reg(PTR_SIZE, A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Lea(A_Reg(PTR_SIZE, A_RegName.R1), A_MemOffset(PTR_SIZE, A_Reg(PTR_SIZE, A_RegName.InstrPtr), A_OffsetLbl(A_DataLabel(PRINTLN_LBL_STR_NAME))))
    program += A_Call(A_ExternalLabel(PUTS))
    program += A_MovTo(A_Reg(PTR_SIZE, A_RegName.R1), A_Imm(ZERO_IMM))
    program += A_Call(A_ExternalLabel(F_FLUSH))
    program += A_MovTo(A_Reg(PTR_SIZE, A_RegName.StackPtr), A_Reg(PTR_SIZE, A_RegName.BasePtr))
    program += A_Pop(A_Reg(PTR_SIZE, A_RegName.BasePtr))
    program += A_Ret

    A_Func(A_InstrLabel(PRINTLN_LABEL), program.toList)
}

inline def defaultPrinti: A_Func = {
    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_Push(A_Reg(PTR_SIZE, A_RegName.BasePtr))
    program += A_MovTo(A_Reg(PTR_SIZE, A_RegName.BasePtr), A_Reg(PTR_SIZE, A_RegName.StackPtr))
    program += A_And(A_Reg(PTR_SIZE, A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_MovTo(A_Reg(INT_SIZE, A_RegName.R2), A_Reg(INT_SIZE, A_RegName.R1))
    program += A_Lea(A_Reg(PTR_SIZE, A_RegName.R1), A_MemOffset(PTR_SIZE, A_Reg(PTR_SIZE, A_RegName.InstrPtr), A_OffsetLbl(A_DataLabel(PRINTI_LBL_STR_NAME))))
    program += A_MovTo(A_Reg(BYTE_SIZE, A_RegName.RetReg), A_Imm(ZERO_IMM))
    program += A_Call(A_ExternalLabel(PRINTF))
    program += A_MovTo(A_Reg(PTR_SIZE, A_RegName.R1), A_Imm(ZERO_IMM))
    program += A_Call(A_ExternalLabel(F_FLUSH))
    program += A_MovTo(A_Reg(PTR_SIZE, A_RegName.StackPtr), A_Reg(PTR_SIZE, A_RegName.BasePtr))
    program += A_Pop(A_Reg(PTR_SIZE, A_RegName.BasePtr))
    program += A_Ret

    A_Func(A_InstrLabel(PRINTI_LABEL), program.toList)
}

inline def defaultPrintc: A_Func = {
    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_Push(A_Reg(PTR_SIZE, A_RegName.BasePtr))
    program += A_MovTo(A_Reg(PTR_SIZE, A_RegName.BasePtr), A_Reg(PTR_SIZE, A_RegName.StackPtr))
    program += A_And(A_Reg(PTR_SIZE, A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_MovTo(A_Reg(CHAR_SIZE, A_RegName.R2), A_Reg(CHAR_SIZE, A_RegName.R1))
    program += A_Lea(A_Reg(PTR_SIZE, A_RegName.R1), A_MemOffset(PTR_SIZE, A_Reg(PTR_SIZE, A_RegName.InstrPtr), A_OffsetLbl(A_DataLabel(PRINTC_LBL_STR_NAME))))
    program += A_MovTo(A_Reg(BYTE_SIZE, A_RegName.RetReg), A_Imm(ZERO_IMM))
    program += A_Call(A_ExternalLabel(PRINTF))
    program += A_MovTo(A_Reg(PTR_SIZE, A_RegName.R1), A_Imm(ZERO_IMM))
    program += A_Call(A_ExternalLabel(F_FLUSH))
    program += A_MovTo(A_Reg(PTR_SIZE, A_RegName.StackPtr), A_Reg(PTR_SIZE, A_RegName.BasePtr))
    program += A_Pop(A_Reg(PTR_SIZE, A_RegName.BasePtr))
    program += A_Ret

    A_Func(A_InstrLabel(PRINTC_LABEL), program.toList)
}

inline def defaultPrintp: A_Func = {
    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_Push(A_Reg(PTR_SIZE, A_RegName.BasePtr))
    program += A_MovTo(A_Reg(PTR_SIZE, A_RegName.BasePtr), A_Reg(PTR_SIZE, A_RegName.StackPtr))
    program += A_And(A_Reg(PTR_SIZE, A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_MovTo(A_Reg(PTR_SIZE, A_RegName.R2), A_Reg(PTR_SIZE, A_RegName.R1))
    program += A_Lea(A_Reg(PTR_SIZE, A_RegName.R1), A_MemOffset(PTR_SIZE, A_Reg(PTR_SIZE, A_RegName.InstrPtr), A_OffsetLbl(A_DataLabel(PRINTP_LBL_STR_NAME))))
    program += A_MovTo(A_Reg(BYTE_SIZE, A_RegName.RetReg), A_Imm(ZERO_IMM))
    program += A_Call(A_ExternalLabel(PRINTF))
    program += A_MovTo(A_Reg(PTR_SIZE, A_RegName.R1), A_Imm(ZERO_IMM))
    program += A_Call(A_ExternalLabel(F_FLUSH))
    program += A_MovTo(A_Reg(PTR_SIZE, A_RegName.StackPtr), A_Reg(PTR_SIZE, A_RegName.BasePtr))
    program += A_Pop(A_Reg(PTR_SIZE, A_RegName.BasePtr))
    program += A_Ret

    A_Func(A_InstrLabel(PRINTP_LABEL), program.toList)
}

inline def defaultPrintb: A_Func = {
    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_Push(A_Reg(PTR_SIZE, A_RegName.BasePtr))
    program += A_MovTo(A_Reg(PTR_SIZE, A_RegName.BasePtr), A_Reg(PTR_SIZE, A_RegName.StackPtr))
    program += A_And(A_Reg(PTR_SIZE, A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Cmp(A_Reg(BYTE_SIZE, A_RegName.R1), A_Imm(ZERO_IMM), BYTE_SIZE)
    program += A_Jmp(A_InstrLabel(PRINTB_FALSE_LABEL), A_Cond.NEq)
    program += A_Lea(A_Reg(PTR_SIZE, A_RegName.R3), A_MemOffset(PTR_SIZE, A_Reg(PTR_SIZE, A_RegName.InstrPtr), A_OffsetLbl(A_DataLabel(PRINTB_FALSE_LBL_STR_NAME))))
    program += A_Jmp(A_InstrLabel(PRINTB_TRUE_LABEL), A_Cond.Uncond)

    program += A_LabelStart(A_InstrLabel(PRINTB_FALSE_LABEL))
    program += A_Lea(A_Reg(PTR_SIZE, A_RegName.R3), A_MemOffset(PTR_SIZE, A_Reg(PTR_SIZE, A_RegName.InstrPtr), A_OffsetLbl(A_DataLabel(PRINTB_TRUE_LBL_STR_NAME))))

    program += A_LabelStart(A_InstrLabel(PRINTB_TRUE_LABEL))
    program += A_MovTo(A_Reg(INT_SIZE, A_RegName.R2), A_RegDeref(INT_SIZE, A_MemOffset(INT_SIZE, A_Reg(PTR_SIZE, A_RegName.R3), A_OffsetImm(-opSizeToInt(INT_SIZE)))))
    program += A_Lea(A_Reg(PTR_SIZE, A_RegName.R1), A_MemOffset(PTR_SIZE, A_Reg(PTR_SIZE, A_RegName.InstrPtr), A_OffsetLbl(A_DataLabel(PRINTB_LBL_STR_NAME))))
    program += A_MovTo(A_Reg(BYTE_SIZE, A_RegName.RetReg), A_Imm(ZERO_IMM))
    program += A_Call(A_ExternalLabel(PRINTF))
    program += A_MovTo(A_Reg(PTR_SIZE, A_RegName.R1), A_Imm(ZERO_IMM))
    program += A_Call(A_ExternalLabel(F_FLUSH))
    program += A_MovTo(A_Reg(PTR_SIZE, A_RegName.StackPtr), A_Reg(PTR_SIZE, A_RegName.BasePtr))
    program += A_Pop(A_Reg(PTR_SIZE, A_RegName.BasePtr))
    program += A_Ret

    A_Func(A_InstrLabel(PRINTB_LABEL), program.toList)
}

inline def defaultPrints: A_Func = {
    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_Push(A_Reg(PTR_SIZE, A_RegName.BasePtr))
    program += A_MovTo(A_Reg(PTR_SIZE, A_RegName.BasePtr), A_Reg(PTR_SIZE, A_RegName.StackPtr))
    program += A_And(A_Reg(PTR_SIZE, A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_MovTo(A_Reg(PTR_SIZE, A_RegName.R3), A_Reg(PTR_SIZE, A_RegName.R1))
    program += A_MovTo(A_Reg(INT_SIZE, A_RegName.R2), A_RegDeref(INT_SIZE, A_MemOffset(INT_SIZE, A_Reg(PTR_SIZE, A_RegName.R1), A_OffsetImm(-opSizeToInt(INT_SIZE)))))
    program += A_Lea(A_Reg(PTR_SIZE, A_RegName.R1), A_MemOffset(PTR_SIZE, A_Reg(PTR_SIZE, A_RegName.InstrPtr), A_OffsetLbl(A_DataLabel(PRINTS_LBL_STR_NAME))))
    program += A_MovTo(A_Reg(BYTE_SIZE, A_RegName.RetReg), A_Imm(ZERO_IMM))
    program += A_Call(A_ExternalLabel(PRINTF))
    program += A_MovTo(A_Reg(PTR_SIZE, A_RegName.R1), A_Imm(ZERO_IMM))
    program += A_Call(A_ExternalLabel(F_FLUSH))
    program += A_MovTo(A_Reg(PTR_SIZE, A_RegName.StackPtr), A_Reg(PTR_SIZE, A_RegName.BasePtr))
    program += A_Pop(A_Reg(PTR_SIZE, A_RegName.BasePtr))
    program += A_Ret

    A_Func(A_InstrLabel(PRINTS_LABEL), program.toList)
}

inline def defaultBadChar: A_Func = {
    val program: ListBuffer[A_Instr] = ListBuffer()
    
    program += A_And(A_Reg(PTR_SIZE, A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Lea(A_Reg(PTR_SIZE, A_RegName.R1), A_MemOffset(PTR_SIZE, A_Reg(PTR_SIZE, A_RegName.InstrPtr), A_OffsetLbl(A_DataLabel(ERR_BAD_CHAR_STR_NAME))))
    program += A_MovTo(A_Reg(BYTE_SIZE, A_RegName.RetReg), A_Imm(ZERO_IMM))
    program += A_Call(A_ExternalLabel(PRINTF))
    program += A_MovTo(A_Reg(PTR_SIZE, A_RegName.R1), A_Imm(ZERO_IMM))
    program += A_Call(A_ExternalLabel(F_FLUSH))
    program += A_MovTo(A_Reg(BYTE_SIZE, A_RegName.R1), A_Imm(ERR_EXIT_CODE))
    program += A_Call(A_ExternalLabel(EXIT))

    A_Func(A_InstrLabel(ERR_BAD_CHAR_LABEL), program.toList)
}

inline def defaultDivZero: A_Func = {
    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_And(A_Reg(PTR_SIZE, A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Lea(A_Reg(PTR_SIZE, A_RegName.R1), A_MemOffset(PTR_SIZE, A_Reg(PTR_SIZE, A_RegName.InstrPtr), A_OffsetLbl(A_DataLabel(DIV_ZERO_LBL_STR_NAME))))
    program += A_Call(A_InstrLabel(PRINTS_LABEL))
    program += A_MovTo(A_Reg(BYTE_SIZE, A_RegName.R1), A_Imm(ERR_EXIT_CODE))
    program += A_Call(A_ExternalLabel(EXIT))

    A_Func(A_InstrLabel(ERR_DIV_ZERO_LABEL), program.toList)
}

inline def defaultOutOfBounds: A_Func = {
    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_And(A_Reg(PTR_SIZE, A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Lea(A_Reg(PTR_SIZE, A_RegName.R1), A_MemOffset(PTR_SIZE, A_Reg(PTR_SIZE, A_RegName.InstrPtr), A_OffsetLbl(A_DataLabel(OUT_OF_BOUNDS_LBL_STR_NAME))))
    program += A_MovTo(A_Reg(BOOL_SIZE, A_RegName.RetReg), A_Imm(ZERO_IMM))
    program += A_Call(A_ExternalLabel(PRINTF))

    // Put 0 into the 64-bit R1 (rdi) to flush all output streams. Note: PTR_SIZE because first argument of fflush is a ptr
    program += A_MovTo(A_Reg(PTR_SIZE, A_RegName.R1), A_Imm(ZERO_IMM)) 
    program += A_Call(A_ExternalLabel(F_FLUSH))

    program += A_MovTo(A_Reg(BOOL_SIZE, A_RegName.R1), A_Imm(ERR_EXIT_CODE)) 
    program += A_Call(A_ExternalLabel(EXIT))

    A_Func(A_InstrLabel(ERR_OUT_OF_BOUNDS_LABEL), program.toList)
}

inline def defaultArrLoad1: A_Func = {
    // PRE: Index is in RetReg, ptr to array is in R1, and will return result into R1

    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_Push(A_Reg(PTR_SIZE, A_RegName.R1))

    // check we don't have a negative index
    program += A_Cmp(A_Reg(INT_SIZE, A_RegName.RetReg), A_Imm(ZERO_IMM), INT_SIZE)
    program += A_Jmp(A_InstrLabel(ERR_OUT_OF_BOUNDS_LABEL), A_Cond.Lt)

    A_MovFromDeref(A_Reg(INT_SIZE, A_RegName.R2), A_RegDeref(INT_SIZE, A_MemOffset(INT_SIZE, A_Reg(PTR_SIZE, A_RegName.RetReg), A_OffsetImm(-opSizeToInt(INT_SIZE)))))

    // check our index isn't >= length:
    program += A_Cmp(A_Reg(INT_SIZE, A_RegName.RetReg), A_Reg(INT_SIZE, A_RegName.R2), INT_SIZE)
    program += A_Jmp(A_InstrLabel(ERR_OUT_OF_BOUNDS_LABEL), A_Cond.GEq)

    program += A_Mul(A_Reg(PTR_SIZE, A_RegName.RetReg), A_Imm(opSizeToInt(INT_SIZE)), PTR_SIZE)
    program += A_MovFromDeref(A_Reg(BOOL_SIZE, A_RegName.R2), A_RegDeref(BOOL_SIZE, A_MemOffset(INT_SIZE, A_Reg(PTR_SIZE, A_RegName.RetReg), A_OffsetReg(A_Reg(PTR_SIZE, A_RegName.RetReg)))))

    program += A_Pop(A_Reg(PTR_SIZE, A_RegName.R1))
    program += A_Ret

    A_Func(A_InstrLabel(ARR_LD1_LABEL), program.toList)
}

inline def defaultArrLoad4: A_Func = {
    // PRE: Index is in RetReg, ptr to array is in R1, and will return result into R1

    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_Push(A_Reg(PTR_SIZE, A_RegName.R1))

    // check we don't have a negative index
    program += A_Cmp(A_Reg(INT_SIZE, A_RegName.RetReg), A_Imm(ZERO_IMM), INT_SIZE)
    program += A_Jmp(A_InstrLabel(ERR_OUT_OF_BOUNDS_LABEL), A_Cond.Lt)

    A_MovFromDeref(A_Reg(INT_SIZE, A_RegName.R2), A_RegDeref(INT_SIZE, A_MemOffset(INT_SIZE, A_Reg(PTR_SIZE, A_RegName.RetReg), A_OffsetImm(-opSizeToInt(INT_SIZE)))))

    // check our index isn't >= length:
    program += A_Cmp(A_Reg(INT_SIZE, A_RegName.RetReg), A_Reg(INT_SIZE, A_RegName.R2), INT_SIZE)
    program += A_Jmp(A_InstrLabel(ERR_OUT_OF_BOUNDS_LABEL), A_Cond.GEq)

    program += A_Mul(A_Reg(PTR_SIZE, A_RegName.RetReg), A_Imm(opSizeToInt(INT_SIZE)), PTR_SIZE)
    program += A_MovFromDeref(A_Reg(INT_SIZE, A_RegName.R2), A_RegDeref(INT_SIZE, A_MemOffset(INT_SIZE, A_Reg(PTR_SIZE, A_RegName.RetReg), A_OffsetReg(A_Reg(PTR_SIZE, A_RegName.RetReg)))))

    program += A_Pop(A_Reg(PTR_SIZE, A_RegName.R1))
    program += A_Ret

    A_Func(A_InstrLabel(ARR_LD4_LABEL), program.toList)
}

inline def defaultArrLoad8: A_Func = {
    // PRE: Index is in RetReg, ptr to array is in R1, and will return result into R1

    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_Push(A_Reg(PTR_SIZE, A_RegName.R1))

    // check we don't have a negative index
    program += A_Cmp(A_Reg(INT_SIZE, A_RegName.RetReg), A_Imm(ZERO_IMM), INT_SIZE)
    program += A_Jmp(A_InstrLabel(ERR_OUT_OF_BOUNDS_LABEL), A_Cond.Lt)

    A_MovFromDeref(A_Reg(INT_SIZE, A_RegName.R2), A_RegDeref(INT_SIZE, A_MemOffset(INT_SIZE, A_Reg(PTR_SIZE, A_RegName.RetReg), A_OffsetImm(-opSizeToInt(INT_SIZE)))))

    // check our index isn't >= length:
    program += A_Cmp(A_Reg(INT_SIZE, A_RegName.RetReg), A_Reg(INT_SIZE, A_RegName.R2), INT_SIZE)
    program += A_Jmp(A_InstrLabel(ERR_OUT_OF_BOUNDS_LABEL), A_Cond.GEq)

    program += A_Mul(A_Reg(PTR_SIZE, A_RegName.RetReg), A_Imm(opSizeToInt(INT_SIZE)), PTR_SIZE)
    program += A_MovFromDeref(A_Reg(PTR_SIZE, A_RegName.R2), A_RegDeref(PTR_SIZE, A_MemOffset(INT_SIZE, A_Reg(PTR_SIZE, A_RegName.RetReg), A_OffsetReg(A_Reg(PTR_SIZE, A_RegName.RetReg)))))

    program += A_Pop(A_Reg(PTR_SIZE, A_RegName.R1))
    program += A_Ret

    A_Func(A_InstrLabel(ARR_LD8_LABEL), program.toList)
}

inline def defaultOutOfMemory: A_Func = {
    val program: ListBuffer[A_Instr] = ListBuffer()
    
    program += A_And(A_Reg(PTR_SIZE, A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Lea(A_Reg(PTR_SIZE, A_RegName.R1), A_MemOffset(PTR_SIZE, A_Reg(PTR_SIZE, A_RegName.InstrPtr), A_OffsetLbl(A_DataLabel(OUT_OF_MEMORY_LBL_STR_NAME))))
    program += A_Call(A_InstrLabel(PRINTS_LABEL))
    program += A_MovTo(A_Reg(BOOL_SIZE, A_RegName.R1), A_Imm(ERR_EXIT_CODE))
    program += A_Call(A_ExternalLabel("exit"))

    A_Func(A_InstrLabel(ERR_OUT_OF_MEMORY_LABEL), program.toList)
}

inline def defaultMalloc: A_Func = {
    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_Push(A_Reg(PTR_SIZE, A_RegName.BasePtr))
    program += A_MovTo(A_Reg(PTR_SIZE, A_RegName.BasePtr), A_Reg(PTR_SIZE, A_RegName.StackPtr))
    program += A_And(A_Reg(PTR_SIZE, A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Call(A_ExternalLabel("malloc"))
    program += A_Cmp(A_Reg(PTR_SIZE, A_RegName.RetReg), A_Imm(ZERO_IMM), PTR_SIZE)
    program += A_Jmp(A_InstrLabel(ERR_OUT_OF_MEMORY_LABEL), A_Cond.Eq)
    program += A_MovTo(A_Reg(PTR_SIZE, A_RegName.StackPtr), A_Reg(PTR_SIZE, A_RegName.BasePtr))
    program += A_Pop(A_Reg(PTR_SIZE, A_RegName.BasePtr))
    program += A_Ret

    A_Func(A_InstrLabel(MALLOC_LABEL), program.toList)
}

inline def defaultFree: A_Func = {
    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_Push(A_Reg(PTR_SIZE, A_RegName.BasePtr))
    program += A_MovTo(A_Reg(PTR_SIZE, A_RegName.BasePtr), A_Reg(PTR_SIZE, A_RegName.StackPtr))
    program += A_And(A_Reg(PTR_SIZE, A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Call(A_ExternalLabel("free"))
    program += A_MovTo(A_Reg(PTR_SIZE, A_RegName.StackPtr), A_Reg(PTR_SIZE, A_RegName.BasePtr))
    program += A_Pop(A_Reg(PTR_SIZE, A_RegName.BasePtr))
    program += A_Ret

    A_Func(A_InstrLabel(FREE_LABEL), program.toList)
}