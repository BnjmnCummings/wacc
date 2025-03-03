package wacc.codeGen

import wacc.assemblyIR.*
import scala.collection.mutable.ListBuffer

// stack aligning for 16 bytes
inline def STACK_ALIGN_VAL = -16
val ERR_EXIT_CODE = -1

val BYTE_SIZE = A_OperandSize.A_8

val OVERFLOW_LBL_STR = "fatal error: integer overflow or underflow occurred"
val OVERFLOW_LBL_STR_NAME = ".L._errOverflow_str"

val DIV_ZERO_LBL_STR = "fatal error: division or modulo by zero"
val DIV_ZERO_LBL_STR_NAME = ".L._errDivZero_str"

val OUT_OF_BOUNDS_LBL_STR = "Error: Array index out of bounds"
val OUT_OF_BOUNDS_LBL_STR_NAME = ".L._errOutOfBounds_str"

val OUT_OF_MEMORY_LBL_STR = "Error: Out of memory"
val OUT_OF_MEMORY_LBL_STR_NAME = ".L._errOutOfMemory_str"

val PRINTLN_LBL_STR = ""
val PRINTLN_LBL_STR_NAME = ".L._println_str"

val PRINTI_LBL_STR = "%d"
val PRINTI_LBL_STR_NAME = ".L._printi_int"

val PRINTC_LBL_STR = "%c"
val PRINTC_LBL_STR_NAME = ".L._printc_str"

val PRINTP_LBL_STR = "%p"
val PRINTP_LBL_STR_NAME = ".L._printp_str"

val PRINTB_TRUE_LBL_STR = "true"
val PRINTB_TRUE_LBL_STR_NAME = ".L._printb_str_true"

val PRINTB_FALSE_LBL_STR = "false"
val PRINTB_FALSE_LBL_STR_NAME = ".L._printb_str_false"

val PRINTB_LBL_STR = "%.*s"
val PRINTB_LBL_STR_NAME = ".L._printb_str"

val PRINTS_LBL_STR = "%.*s"
val PRINTS_LBL_STR_NAME = ".L._prints_str"

val READI_LBL_STR = "%d"
val READI_LBL_STR_NAME = ".L._readi_str"

val READC_LBL_STR = "%c"
val READC_LBL_STR_NAME = ".L._readc_str"

val ERR_BAD_CHAR_STR = "fatal error: int %d is not ascii character 0-127"
val ERR_BAD_CHAR_STR_NAME = ".L._errBadChar_str"

val F_FLUSH = "fflush"
val PUTS = "puts"
val EXIT = "exit"
val PRINTF = "printf"
val SCANF = "scanf"

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
val READI_LABEL = "_readi"
val READC_LABEL = "_readc"
val EXIT_LABEL = "_exit"
val ARR_LD1_LABEL = "_arrLoad1"
val ARR_LD4_LABEL = "_arrLoad4"
val ARR_LD8_LABEL = "_arrLoad8"
val MALLOC_LABEL = "_malloc"
val FREE_LABEL = "_free"
val FREE_PAIR_LABEL = "_freePair"

def defaultFuncsFuncDependency: Map[String, Set[String]] = Map(
    ERR_OVERFLOW_LABEL -> Set(PRINTS_LABEL),
    ERR_OUT_OF_BOUNDS_LABEL -> Set(PRINTS_LABEL),
    ERR_OUT_OF_MEMORY_LABEL -> Set(PRINTS_LABEL),
    ERR_DIV_ZERO_LABEL -> Set(PRINTS_LABEL),
    ERR_BAD_CHAR_LABEL -> Set(PRINTS_LABEL),
    PRINTLN_LABEL -> Set(),
    PRINTI_LABEL -> Set(),
    PRINTC_LABEL -> Set(),
    PRINTP_LABEL -> Set(),
    PRINTB_LABEL -> Set(),
    PRINTS_LABEL -> Set(),
    READC_LABEL -> Set(),
    READI_LABEL -> Set(),
    EXIT_LABEL -> Set(),
    ARR_LD1_LABEL -> Set(ERR_OUT_OF_BOUNDS_LABEL),
    ARR_LD4_LABEL -> Set(ERR_OUT_OF_BOUNDS_LABEL),
    ARR_LD8_LABEL -> Set(ERR_OUT_OF_BOUNDS_LABEL),
    MALLOC_LABEL -> Set(ERR_OUT_OF_MEMORY_LABEL),
    FREE_LABEL -> Set(),
    FREE_PAIR_LABEL -> Set(ERR_OUT_OF_MEMORY_LABEL)
)

def defaultFuncsStrDependency: Map[String, Set[(String, String)]] = Map(
    ERR_OVERFLOW_LABEL -> Set((OVERFLOW_LBL_STR_NAME, OVERFLOW_LBL_STR)),
    ERR_OUT_OF_BOUNDS_LABEL -> Set((OUT_OF_BOUNDS_LBL_STR_NAME, OUT_OF_BOUNDS_LBL_STR)),
    ERR_OUT_OF_MEMORY_LABEL -> Set((OUT_OF_MEMORY_LBL_STR_NAME, OUT_OF_MEMORY_LBL_STR)),
    ERR_DIV_ZERO_LABEL -> Set((DIV_ZERO_LBL_STR_NAME, DIV_ZERO_LBL_STR)),
    ERR_BAD_CHAR_LABEL -> Set((ERR_BAD_CHAR_STR_NAME, ERR_BAD_CHAR_STR)),
    PRINTLN_LABEL -> Set((PRINTLN_LBL_STR_NAME, PRINTLN_LBL_STR)),
    PRINTI_LABEL -> Set((PRINTI_LBL_STR_NAME, PRINTI_LBL_STR)),
    PRINTC_LABEL -> Set((PRINTC_LBL_STR_NAME, PRINTC_LBL_STR)),
    PRINTP_LABEL -> Set((PRINTP_LBL_STR_NAME, PRINTP_LBL_STR)),
    PRINTB_LABEL -> Set((PRINTB_LBL_STR_NAME, PRINTB_LBL_STR), (PRINTB_TRUE_LBL_STR_NAME, PRINTB_TRUE_LBL_STR), (PRINTB_FALSE_LBL_STR_NAME, PRINTB_FALSE_LBL_STR)),
    PRINTS_LABEL -> Set((PRINTS_LBL_STR_NAME, PRINTS_LBL_STR)),
    READI_LABEL -> Set((READI_LBL_STR_NAME, READI_LBL_STR)),
    READC_LABEL -> Set((READC_LBL_STR_NAME, READC_LBL_STR)),
    EXIT_LABEL -> Set(),
    ARR_LD1_LABEL -> Set(),
    ARR_LD4_LABEL -> Set(),
    ARR_LD8_LABEL -> Set(),
    MALLOC_LABEL -> Set(),
    FREE_LABEL -> Set(),
    FREE_PAIR_LABEL -> Set()
)  

def defaultFuncsLabelToFunc: Map[String, A_Func] = Map(
    ERR_OVERFLOW_LABEL -> defaultOverflow,
    ERR_OUT_OF_BOUNDS_LABEL -> defaultOutOfBounds,
    ERR_OUT_OF_MEMORY_LABEL -> defaultOutOfMemory,
    ERR_DIV_ZERO_LABEL -> defaultDivZero,
    ERR_BAD_CHAR_LABEL -> defaultBadChar,
    PRINTLN_LABEL -> defaultPrintln,
    PRINTI_LABEL -> defaultPrinti,
    PRINTC_LABEL -> defaultPrintc,
    PRINTP_LABEL -> defaultPrintp,
    PRINTB_LABEL -> defaultPrintb,
    PRINTS_LABEL -> defaultPrints,
    READI_LABEL -> defaultReadi,
    READC_LABEL -> defaultReadc,
    EXIT_LABEL -> defaultExit,
    MALLOC_LABEL -> defaultMalloc,
    FREE_LABEL -> defaultFree,
    FREE_PAIR_LABEL -> defaultFreePair
)

inline def defaultExit: A_Func = {
    val program: ListBuffer[A_Instr] = ListBuffer()
    program += A_Push(A_Reg(A_RegName.BasePtr))
    program += A_MovTo(A_Reg(A_RegName.BasePtr), A_Reg(A_RegName.StackPtr), PTR_SIZE)
    program += A_And(A_Reg(A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Call(A_ExternalLabel(EXIT))
    program += A_MovTo(A_Reg(A_RegName.StackPtr), A_Reg(A_RegName.BasePtr), PTR_SIZE)
    program += A_Pop(A_Reg(A_RegName.BasePtr))
    program += A_Ret

    A_Func(A_InstrLabel(EXIT_LABEL), program.toList)
}

inline def defaultOverflow: A_Func = {
    val program: ListBuffer[A_Instr] = ListBuffer()
    program += A_And(A_Reg(A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Lea(A_Reg(A_RegName.R1), A_MemOffset(A_Reg(A_RegName.InstrPtr), A_OffsetLbl(A_DataLabel(OVERFLOW_LBL_STR_NAME))))
    program += A_Call(A_InstrLabel(PRINTS_LABEL))
    program += A_MovTo(A_Reg(A_RegName.R1), A_Imm(ERR_EXIT_CODE), EXIT_CODE_SIZE)
    program += A_Call(A_ExternalLabel(EXIT))

    A_Func(A_InstrLabel(ERR_OVERFLOW_LABEL), program.toList)
}

// When calling print:
    // edi holds the format string
    // esi holds the value to be printed

inline def defaultPrintln: A_Func = {
    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_Push(A_Reg(A_RegName.BasePtr))
    program += A_MovTo(A_Reg(A_RegName.BasePtr), A_Reg(A_RegName.StackPtr), PTR_SIZE)
    program += A_And(A_Reg(A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Lea(A_Reg(A_RegName.R1), A_MemOffset(A_Reg(A_RegName.InstrPtr), A_OffsetLbl(A_DataLabel(PRINTLN_LBL_STR_NAME))))
    program += A_Call(A_ExternalLabel(PUTS))
    program += A_MovTo(A_Reg(A_RegName.R1), A_Imm(ZERO_IMM), PTR_SIZE)
    program += A_Call(A_ExternalLabel(F_FLUSH))
    program += A_MovTo(A_Reg(A_RegName.StackPtr), A_Reg(A_RegName.BasePtr), PTR_SIZE)
    program += A_Pop(A_Reg(A_RegName.BasePtr))
    program += A_Ret

    A_Func(A_InstrLabel(PRINTLN_LABEL), program.toList)
}

inline def defaultPrinti: A_Func = {
    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_Push(A_Reg(A_RegName.BasePtr))
    program += A_MovTo(A_Reg(A_RegName.BasePtr), A_Reg(A_RegName.StackPtr), PTR_SIZE)
    program += A_And(A_Reg(A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_MovTo(A_Reg(A_RegName.R2), A_Reg(A_RegName.R1), INT_SIZE)
    program += A_Lea(A_Reg(A_RegName.R1), A_MemOffset(A_Reg(A_RegName.InstrPtr), A_OffsetLbl(A_DataLabel(PRINTI_LBL_STR_NAME))))
    program += A_MovTo(A_Reg(A_RegName.RetReg), A_Imm(ZERO_IMM), BYTE_SIZE)
    program += A_Call(A_ExternalLabel(PRINTF))
    program += A_MovTo(A_Reg(A_RegName.R1), A_Imm(ZERO_IMM), PTR_SIZE)
    program += A_Call(A_ExternalLabel(F_FLUSH))
    program += A_MovTo(A_Reg(A_RegName.StackPtr), A_Reg(A_RegName.BasePtr), PTR_SIZE)
    program += A_Pop(A_Reg(A_RegName.BasePtr))
    program += A_Ret

    A_Func(A_InstrLabel(PRINTI_LABEL), program.toList)
}

inline def defaultPrintc: A_Func = {
    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_Push(A_Reg(A_RegName.BasePtr))
    program += A_MovTo(A_Reg(A_RegName.BasePtr), A_Reg(A_RegName.StackPtr), PTR_SIZE)
    program += A_And(A_Reg(A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_MovTo(A_Reg(A_RegName.R2), A_Reg(A_RegName.R1), CHAR_SIZE)
    program += A_Lea(A_Reg(A_RegName.R1), A_MemOffset(A_Reg(A_RegName.InstrPtr), A_OffsetLbl(A_DataLabel(PRINTC_LBL_STR_NAME))))
    program += A_MovTo(A_Reg(A_RegName.RetReg), A_Imm(ZERO_IMM), BYTE_SIZE)
    program += A_Call(A_ExternalLabel(PRINTF))
    program += A_MovTo(A_Reg(A_RegName.R1), A_Imm(ZERO_IMM), PTR_SIZE)
    program += A_Call(A_ExternalLabel(F_FLUSH))
    program += A_MovTo(A_Reg(A_RegName.StackPtr), A_Reg(A_RegName.BasePtr), PTR_SIZE)
    program += A_Pop(A_Reg(A_RegName.BasePtr))
    program += A_Ret

    A_Func(A_InstrLabel(PRINTC_LABEL), program.toList)
}

inline def defaultPrintp: A_Func = {
    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_Push(A_Reg(A_RegName.BasePtr))
    program += A_MovTo(A_Reg(A_RegName.BasePtr), A_Reg(A_RegName.StackPtr), PTR_SIZE)
    program += A_And(A_Reg(A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_MovTo(A_Reg(A_RegName.R2), A_Reg(A_RegName.R1), PTR_SIZE)
    program += A_Lea(A_Reg(A_RegName.R1), A_MemOffset(A_Reg(A_RegName.InstrPtr), A_OffsetLbl(A_DataLabel(PRINTP_LBL_STR_NAME))))
    program += A_MovTo(A_Reg(A_RegName.RetReg), A_Imm(ZERO_IMM), BYTE_SIZE)
    program += A_Call(A_ExternalLabel(PRINTF))
    program += A_MovTo(A_Reg(A_RegName.R1), A_Imm(ZERO_IMM), PTR_SIZE)
    program += A_Call(A_ExternalLabel(F_FLUSH))
    program += A_MovTo(A_Reg(A_RegName.StackPtr), A_Reg(A_RegName.BasePtr), PTR_SIZE)
    program += A_Pop(A_Reg(A_RegName.BasePtr))
    program += A_Ret

    A_Func(A_InstrLabel(PRINTP_LABEL), program.toList)
}

inline def defaultPrintb: A_Func = {
    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_Push(A_Reg(A_RegName.BasePtr))
    program += A_MovTo(A_Reg(A_RegName.BasePtr), A_Reg(A_RegName.StackPtr), PTR_SIZE)
    program += A_And(A_Reg(A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Cmp(A_Reg(A_RegName.R1), A_Imm(ZERO_IMM), BYTE_SIZE)
    program += A_Jmp(A_InstrLabel(PRINTB_FALSE_LABEL), A_Cond.NEq)
    program += A_Lea(A_Reg(A_RegName.R3), A_MemOffset(A_Reg(A_RegName.InstrPtr), A_OffsetLbl(A_DataLabel(PRINTB_FALSE_LBL_STR_NAME))))
    program += A_Jmp(A_InstrLabel(PRINTB_TRUE_LABEL), A_Cond.Uncond)

    program += A_LabelStart(A_InstrLabel(PRINTB_FALSE_LABEL))
    program += A_Lea(A_Reg(A_RegName.R3), A_MemOffset(A_Reg(A_RegName.InstrPtr), A_OffsetLbl(A_DataLabel(PRINTB_TRUE_LBL_STR_NAME))))

    program += A_LabelStart(A_InstrLabel(PRINTB_TRUE_LABEL))
    program += A_MovTo(A_Reg(A_RegName.R2), A_RegDeref(A_MemOffset(A_Reg(A_RegName.R3), A_OffsetImm(-opSizeToInt(INT_SIZE)))), INT_SIZE)
    program += A_Lea(A_Reg(A_RegName.R1), A_MemOffset(A_Reg(A_RegName.InstrPtr), A_OffsetLbl(A_DataLabel(PRINTB_LBL_STR_NAME))))
    program += A_MovTo(A_Reg(A_RegName.RetReg), A_Imm(ZERO_IMM), BYTE_SIZE)
    program += A_Call(A_ExternalLabel(PRINTF))
    program += A_MovTo(A_Reg(A_RegName.R1), A_Imm(ZERO_IMM), PTR_SIZE)
    program += A_Call(A_ExternalLabel(F_FLUSH))
    program += A_MovTo(A_Reg(A_RegName.StackPtr), A_Reg(A_RegName.BasePtr), PTR_SIZE)
    program += A_Pop(A_Reg(A_RegName.BasePtr))
    program += A_Ret

    A_Func(A_InstrLabel(PRINTB_LABEL), program.toList)
}

inline def defaultPrints: A_Func = {
    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_Push(A_Reg(A_RegName.BasePtr))
    program += A_MovTo(A_Reg(A_RegName.BasePtr), A_Reg(A_RegName.StackPtr), PTR_SIZE)
    program += A_And(A_Reg(A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_MovTo(A_Reg(A_RegName.R3), A_Reg(A_RegName.R1), PTR_SIZE)
    program += A_MovTo(A_Reg(A_RegName.R2), A_RegDeref(A_MemOffset(A_Reg(A_RegName.R1), A_OffsetImm(-opSizeToInt(INT_SIZE)))), INT_SIZE)
    program += A_Lea(A_Reg(A_RegName.R1), A_MemOffset( A_Reg(A_RegName.InstrPtr), A_OffsetLbl(A_DataLabel(PRINTS_LBL_STR_NAME))))
    program += A_MovTo(A_Reg(A_RegName.RetReg), A_Imm(ZERO_IMM), BYTE_SIZE)
    program += A_Call(A_ExternalLabel(PRINTF))
    program += A_MovTo(A_Reg(A_RegName.R1), A_Imm(ZERO_IMM), PTR_SIZE)
    program += A_Call(A_ExternalLabel(F_FLUSH))
    program += A_MovTo(A_Reg(A_RegName.StackPtr), A_Reg(A_RegName.BasePtr), PTR_SIZE)
    program += A_Pop(A_Reg(A_RegName.BasePtr))
    program += A_Ret

    A_Func(A_InstrLabel(PRINTS_LABEL), program.toList)
}

inline def defaultReadc: A_Func = {
    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_Push(A_Reg(A_RegName.BasePtr))
    program += A_MovTo(A_Reg(A_RegName.BasePtr), A_Reg(A_RegName.StackPtr), PTR_SIZE)
    program += A_And(A_Reg(A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Sub(A_Reg(A_RegName.StackPtr), A_Imm(16), PTR_SIZE)
    program += A_Lea(A_Reg(A_RegName.R2), A_MemOffset(A_Reg(A_RegName.StackPtr), A_OffsetImm(0)))
    program += A_Lea(A_Reg(A_RegName.R1), A_MemOffset(A_Reg(A_RegName.InstrPtr), A_OffsetLbl(A_DataLabel(READC_LBL_STR_NAME))))
    program += A_MovTo(A_Reg(A_RegName.RetReg), A_Imm(ZERO_IMM), BYTE_SIZE)
    program += A_Call(A_ExternalLabel(SCANF))
    program += A_MovFromDeref(A_Reg(A_RegName.RetReg), A_RegDeref(A_MemOffset(A_Reg(A_RegName.StackPtr), A_OffsetImm(0))), CHAR_SIZE)
    program += A_Add(A_Reg(A_RegName.StackPtr), A_Imm(16), PTR_SIZE)
    program += A_MovTo(A_Reg(A_RegName.StackPtr), A_Reg(A_RegName.BasePtr), PTR_SIZE)
    program += A_Pop(A_Reg(A_RegName.BasePtr))

    A_Func(A_InstrLabel(READC_LABEL), program.toList)
}

inline def defaultReadi: A_Func = {
    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_Push(A_Reg(A_RegName.BasePtr))
    program += A_MovTo(A_Reg(A_RegName.BasePtr), A_Reg(A_RegName.StackPtr), PTR_SIZE)
    program += A_And(A_Reg(A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Sub(A_Reg(A_RegName.StackPtr), A_Imm(16), PTR_SIZE)
    program += A_Lea(A_Reg(A_RegName.R2), A_MemOffset(A_Reg(A_RegName.StackPtr), A_OffsetImm(0)))
    program += A_Lea(A_Reg(A_RegName.R1), A_MemOffset(A_Reg(A_RegName.InstrPtr), A_OffsetLbl(A_DataLabel(READI_LBL_STR_NAME))))
    program += A_MovTo(A_Reg(A_RegName.RetReg), A_Imm(ZERO_IMM), BYTE_SIZE)
    program += A_Call(A_ExternalLabel(SCANF))
    program += A_MovFromDeref(A_Reg(A_RegName.RetReg), A_RegDeref(A_MemOffset(A_Reg(A_RegName.StackPtr), A_OffsetImm(0))), INT_SIZE)
    program += A_Add(A_Reg(A_RegName.StackPtr), A_Imm(16), PTR_SIZE)
    program += A_MovTo(A_Reg(A_RegName.StackPtr), A_Reg(A_RegName.BasePtr), PTR_SIZE)
    program += A_Pop(A_Reg(A_RegName.BasePtr))

    A_Func(A_InstrLabel(READI_LABEL), program.toList)
}

inline def defaultBadChar: A_Func = {
    val program: ListBuffer[A_Instr] = ListBuffer()
    
    program += A_And(A_Reg(A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Lea(A_Reg(A_RegName.R1), A_MemOffset(A_Reg(A_RegName.InstrPtr), A_OffsetLbl(A_DataLabel(ERR_BAD_CHAR_STR_NAME))))
    program += A_MovTo(A_Reg(A_RegName.RetReg), A_Imm(ZERO_IMM), BYTE_SIZE)
    program += A_Call(A_ExternalLabel(PRINTF))
    program += A_MovTo(A_Reg(A_RegName.R1), A_Imm(ZERO_IMM), PTR_SIZE)
    program += A_Call(A_ExternalLabel(F_FLUSH))
    program += A_MovTo(A_Reg(A_RegName.R1), A_Imm(ERR_EXIT_CODE), BYTE_SIZE)
    program += A_Call(A_ExternalLabel(EXIT))

    A_Func(A_InstrLabel(ERR_BAD_CHAR_LABEL), program.toList)
}

inline def defaultDivZero: A_Func = {
    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_And(A_Reg(A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Lea(A_Reg(A_RegName.R1), A_MemOffset(A_Reg(A_RegName.InstrPtr), A_OffsetLbl(A_DataLabel(DIV_ZERO_LBL_STR_NAME))))
    program += A_Call(A_InstrLabel(PRINTS_LABEL))
    program += A_MovTo(A_Reg(A_RegName.R1), A_Imm(ERR_EXIT_CODE), BYTE_SIZE)
    program += A_Call(A_ExternalLabel(EXIT))

    A_Func(A_InstrLabel(ERR_DIV_ZERO_LABEL), program.toList)
}

inline def defaultOutOfBounds: A_Func = {
    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_And(A_Reg(A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Lea(A_Reg(A_RegName.R1), A_MemOffset(A_Reg(A_RegName.InstrPtr), A_OffsetLbl(A_DataLabel(OUT_OF_BOUNDS_LBL_STR_NAME))))
    program += A_MovTo(A_Reg(A_RegName.RetReg), A_Imm(ZERO_IMM), BOOL_SIZE)
    program += A_Call(A_ExternalLabel(PRINTF))

    // Put 0 into the 64-bit R1 (rdi) to flush all output streams. Note: PTR_SIZE because first argument of fflush is a ptr
    program += A_MovTo(A_Reg(A_RegName.R1), A_Imm(ZERO_IMM), PTR_SIZE) 
    program += A_Call(A_ExternalLabel(F_FLUSH))

    program += A_MovTo(A_Reg(A_RegName.R1), A_Imm(ERR_EXIT_CODE), BOOL_SIZE) 
    program += A_Call(A_ExternalLabel(EXIT))

    A_Func(A_InstrLabel(ERR_OUT_OF_BOUNDS_LABEL), program.toList)
}

inline def defaultOutOfMemory: A_Func = {
    val program: ListBuffer[A_Instr] = ListBuffer()
    
    program += A_And(A_Reg(A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Lea(A_Reg(A_RegName.R1), A_MemOffset(A_Reg(A_RegName.InstrPtr), A_OffsetLbl(A_DataLabel(OUT_OF_MEMORY_LBL_STR_NAME))))
    program += A_Call(A_InstrLabel(PRINTS_LABEL))
    program += A_MovTo(A_Reg(A_RegName.R1), A_Imm(ERR_EXIT_CODE), BOOL_SIZE)
    program += A_Call(A_ExternalLabel("exit"))

    A_Func(A_InstrLabel(ERR_OUT_OF_MEMORY_LABEL), program.toList)
}

inline def defaultMalloc: A_Func = {
    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_Push(A_Reg(A_RegName.BasePtr))
    program += A_MovTo(A_Reg(A_RegName.BasePtr), A_Reg(A_RegName.StackPtr), PTR_SIZE)
    program += A_And(A_Reg(A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Call(A_ExternalLabel("malloc"))
    program += A_Cmp(A_Reg(A_RegName.RetReg), A_Imm(ZERO_IMM), PTR_SIZE)
    program += A_Jmp(A_InstrLabel(ERR_OUT_OF_MEMORY_LABEL), A_Cond.Eq)
    program += A_MovTo(A_Reg(A_RegName.StackPtr), A_Reg(A_RegName.BasePtr), PTR_SIZE)
    program += A_Pop(A_Reg(A_RegName.BasePtr))
    program += A_Ret

    A_Func(A_InstrLabel(MALLOC_LABEL), program.toList)
}

inline def defaultFree: A_Func = {
    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_Push(A_Reg(A_RegName.BasePtr))
    program += A_MovTo(A_Reg(A_RegName.BasePtr), A_Reg(A_RegName.StackPtr), PTR_SIZE)
    program += A_And(A_Reg(A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Call(A_ExternalLabel("free"))
    program += A_MovTo(A_Reg(A_RegName.StackPtr), A_Reg(A_RegName.BasePtr), PTR_SIZE)
    program += A_Pop(A_Reg(A_RegName.BasePtr))
    program += A_Ret

    A_Func(A_InstrLabel(FREE_LABEL), program.toList)
}

inline def defaultFreePair: A_Func = {
    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_Push(A_Reg(A_RegName.BasePtr))
    program += A_MovTo(A_Reg(A_RegName.BasePtr), A_Reg(A_RegName.StackPtr), PTR_SIZE)
    program += A_And(A_Reg(A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Cmp(A_Reg(A_RegName.R1), A_Imm(ZERO_IMM), PTR_SIZE)
    program += A_Jmp(A_InstrLabel(ERR_OUT_OF_MEMORY_LABEL), A_Cond.Eq)
    program += A_Call(A_ExternalLabel("free"))
    program += A_MovTo(A_Reg(A_RegName.StackPtr), A_Reg(A_RegName.BasePtr), PTR_SIZE)
    program += A_Pop(A_Reg(A_RegName.BasePtr))
    program += A_Ret

    A_Func(A_InstrLabel(FREE_PAIR_LABEL), program.toList)
}