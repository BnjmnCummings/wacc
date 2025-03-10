package wacc.codeGen

import wacc.assemblyIR.*

import scala.collection.mutable.ListBuffer

inline def STACK_ALIGN_VAL = -16 /* stack alignment for 16 bytes */
inline def ERR_EXIT_CODE = -1

/**
  * A map of default function labels to their dependencies.
  * @return a map of default function labels to their dependencies
  */
def defaultFuncsFuncDependency: Map[A_DefaultLabel, Set[A_DefaultLabel]] = Map(
    ERR_OVERFLOW_LABEL -> Set(PRINTS_LABEL),
    ERR_OUT_OF_BOUNDS_LABEL -> Set(PRINTS_LABEL),
    ERR_OUT_OF_MEMORY_LABEL -> Set(PRINTS_LABEL),
    ERR_DIV_ZERO_LABEL -> Set(PRINTS_LABEL),
    ERR_BAD_CHAR_LABEL -> Set(PRINTS_LABEL),
    ERR_NULL_PAIR_LABEL -> Set(PRINTS_LABEL),
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
    FREE_LABEL -> Set(ERR_NULL_PAIR_LABEL),
    FREE_PAIR_LABEL -> Set(ERR_OUT_OF_MEMORY_LABEL)
)

/**
  * A map of function labels to their requred string labels.
  * @return a map of default function labels to their dependencies
  */
def defaultFuncsStrDependency: Map[A_DefaultLabel, Set[(A_DataLabel, String)]] = Map(
    ERR_OVERFLOW_LABEL -> Set((OVERFLOW_LBL_STR_NAME, OVERFLOW_LBL_STR)),
    ERR_OUT_OF_BOUNDS_LABEL -> Set((OUT_OF_BOUNDS_LBL_STR_NAME, OUT_OF_BOUNDS_LBL_STR)),
    ERR_OUT_OF_MEMORY_LABEL -> Set((OUT_OF_MEMORY_LBL_STR_NAME, OUT_OF_MEMORY_LBL_STR)),
    ERR_DIV_ZERO_LABEL -> Set((DIV_ZERO_LBL_STR_NAME, DIV_ZERO_LBL_STR)),
    ERR_BAD_CHAR_LABEL -> Set((ERR_BAD_CHAR_STR_NAME, ERR_BAD_CHAR_STR)),
    ERR_NULL_PAIR_LABEL -> Set((ERR_NULL_PAIR_STR_NAME, ERR_NULL_PAIR_STR)),
    PRINTLN_LABEL -> Set((PRINTLN_LBL_STR_NAME, PRINTLN_LBL_STR)),
    PRINTI_LABEL -> Set((PRINTI_LBL_STR_NAME, PRINTI_LBL_STR)),
    PRINTC_LABEL -> Set((PRINTC_LBL_STR_NAME, PRINTC_LBL_STR)),
    PRINTP_LABEL -> Set((PRINTP_LBL_STR_NAME, PRINTP_LBL_STR)),
    PRINTB_LABEL -> Set(
        (PRINTB_LBL_STR_NAME, PRINTB_LBL_STR), 
        (PRINTB_TRUE_LBL_STR_NAME, PRINTB_TRUE_LBL_STR), 
        (PRINTB_FALSE_LBL_STR_NAME, PRINTB_FALSE_LBL_STR)
    ),
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

/**
 * 
 */
def defaultFuncsLabelToFunc: Map[A_DefaultLabel, A_Func] = Map(
    ERR_OVERFLOW_LABEL -> defaultOverflow,
    ERR_OUT_OF_BOUNDS_LABEL -> defaultOutOfBounds,
    ERR_OUT_OF_MEMORY_LABEL -> defaultOutOfMemory,
    ERR_DIV_ZERO_LABEL -> defaultDivZero,
    ERR_BAD_CHAR_LABEL -> defaultBadChar,
    ERR_NULL_PAIR_LABEL -> defaultErrNull,
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

inline def defaultExit: A_Func = 
    val program: ListBuffer[A_Instr] = ListBuffer()
    program += A_Push(A_Reg(A_RegName.BasePtr))
    program += A_Mov(A_Reg(A_RegName.BasePtr), A_Reg(A_RegName.StackPtr), PTR_SIZE)
    program += A_And(A_Reg(A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Call(EXIT)
    program += A_Mov(A_Reg(A_RegName.StackPtr), A_Reg(A_RegName.BasePtr), PTR_SIZE)
    program += A_Pop(A_Reg(A_RegName.BasePtr))
    program += A_Ret

    A_Func(EXIT_LABEL, program.toList)


inline def defaultOverflow: A_Func = 
    val program: ListBuffer[A_Instr] = ListBuffer()
    program += A_And(A_Reg(A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Lea(A_Reg(A_RegName.Arg1), A_MemOffset(A_Reg(A_RegName.InstrPtr), A_OffsetLbl(OVERFLOW_LBL_STR_NAME)))
    program += A_Call(PRINTS_LABEL)
    program += A_Mov(A_Reg(A_RegName.Arg1), A_Imm(ERR_EXIT_CODE), EXIT_CODE_SIZE)
    program += A_Call(EXIT)

    A_Func(ERR_OVERFLOW_LABEL, program.toList)

// When calling print:
    // edi holds the format string
    // esi holds the value to be printed

inline def defPrint(size: A_OperandSize, dataLabel: A_DataLabel, defLabel: A_DefaultLabel): A_Func = 
    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_Push(A_Reg(A_RegName.BasePtr))
    program += A_Mov(A_Reg(A_RegName.BasePtr), A_Reg(A_RegName.StackPtr), PTR_SIZE)
    program += A_And(A_Reg(A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Mov(A_Reg(A_RegName.Arg2), A_Reg(A_RegName.Arg1), size)
    program += A_Lea(A_Reg(A_RegName.Arg1), A_MemOffset(A_Reg(A_RegName.InstrPtr), A_OffsetLbl(dataLabel)))
    program += A_Mov(A_Reg(A_RegName.RetReg), A_Imm(ZERO_IMM), BYTE_SIZE)
    program += A_Call(PRINTF)
    program += A_Mov(A_Reg(A_RegName.Arg1), A_Imm(ZERO_IMM), PTR_SIZE)
    program += A_Call(F_FLUSH)
    program += A_Mov(A_Reg(A_RegName.StackPtr), A_Reg(A_RegName.BasePtr), PTR_SIZE)
    program += A_Pop(A_Reg(A_RegName.BasePtr))
    program += A_Ret

    A_Func(defLabel, program.toList)


inline def defaultPrinti: A_Func = 
    defPrint(INT_SIZE, PRINTI_LBL_STR_NAME, PRINTI_LABEL)

inline def defaultPrintc: A_Func =
    defPrint(CHAR_SIZE, PRINTC_LBL_STR_NAME, PRINTC_LABEL)

inline def defaultPrintp: A_Func = 
    defPrint(PTR_SIZE, PRINTP_LBL_STR_NAME, PRINTP_LABEL)

inline def defaultPrintb: A_Func =
    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_Push(A_Reg(A_RegName.BasePtr))
    program += A_Mov(A_Reg(A_RegName.BasePtr), A_Reg(A_RegName.StackPtr), PTR_SIZE)
    program += A_And(A_Reg(A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Cmp(A_Reg(A_RegName.Arg1), A_Imm(ZERO_IMM), BYTE_SIZE)
    program += A_Jmp(PRINTB_FALSE_LABEL, A_Cond.NEq)
    program += A_Lea(A_Reg(A_RegName.Arg3), A_MemOffset(A_Reg(A_RegName.InstrPtr), A_OffsetLbl(PRINTB_FALSE_LBL_STR_NAME)))
    program += A_Jmp(PRINTB_TRUE_LABEL, A_Cond.Uncond)
    program += A_LabelStart(PRINTB_FALSE_LABEL)
    program += A_Lea(A_Reg(A_RegName.Arg3), A_MemOffset(A_Reg(A_RegName.InstrPtr), A_OffsetLbl(PRINTB_TRUE_LBL_STR_NAME)))
    program += A_LabelStart(PRINTB_TRUE_LABEL)
    program += A_Mov(A_Reg(A_RegName.Arg2), A_RegDeref(A_MemOffset(A_Reg(A_RegName.Arg3), A_OffsetImm(-opSizeToInt(INT_SIZE)))), INT_SIZE)
    program += A_Lea(A_Reg(A_RegName.Arg1), A_MemOffset(A_Reg(A_RegName.InstrPtr), A_OffsetLbl(PRINTB_LBL_STR_NAME)))
    program += A_Mov(A_Reg(A_RegName.RetReg), A_Imm(ZERO_IMM), BYTE_SIZE)
    program += A_Call(PRINTF)
    program += A_Mov(A_Reg(A_RegName.Arg1), A_Imm(ZERO_IMM), PTR_SIZE)
    program += A_Call(F_FLUSH)
    program += A_Mov(A_Reg(A_RegName.StackPtr), A_Reg(A_RegName.BasePtr), PTR_SIZE)
    program += A_Pop(A_Reg(A_RegName.BasePtr))
    program += A_Ret

    A_Func(PRINTB_LABEL, program.toList)

inline def defaultPrints: A_Func = 
    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_Push(A_Reg(A_RegName.BasePtr))
    program += A_Mov(A_Reg(A_RegName.BasePtr), A_Reg(A_RegName.StackPtr), PTR_SIZE)
    program += A_And(A_Reg(A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Mov(A_Reg(A_RegName.Arg3), A_Reg(A_RegName.Arg1), PTR_SIZE)
    program += A_Mov(A_Reg(A_RegName.Arg2), A_RegDeref(A_MemOffset(A_Reg(A_RegName.Arg1), A_OffsetImm(-opSizeToInt(INT_SIZE)))), INT_SIZE)
    program += A_Lea(A_Reg(A_RegName.Arg1), A_MemOffset( A_Reg(A_RegName.InstrPtr), A_OffsetLbl(PRINTS_LBL_STR_NAME)))
    program += A_Mov(A_Reg(A_RegName.RetReg), A_Imm(ZERO_IMM), BYTE_SIZE)
    program += A_Call(PRINTF)
    program += A_Mov(A_Reg(A_RegName.Arg1), A_Imm(ZERO_IMM), PTR_SIZE)
    program += A_Call(F_FLUSH)
    program += A_Mov(A_Reg(A_RegName.StackPtr), A_Reg(A_RegName.BasePtr), PTR_SIZE)
    program += A_Pop(A_Reg(A_RegName.BasePtr))
    program += A_Ret

    A_Func(PRINTS_LABEL, program.toList)

inline def defaultPrintln: A_Func = 
    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_Push(A_Reg(A_RegName.BasePtr))
    program += A_Mov(A_Reg(A_RegName.BasePtr), A_Reg(A_RegName.StackPtr), PTR_SIZE)
    program += A_And(A_Reg(A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Lea(A_Reg(A_RegName.Arg1), A_MemOffset(A_Reg(A_RegName.InstrPtr), A_OffsetLbl(PRINTLN_LBL_STR_NAME)))
    program += A_Call(PUTS)
    program += A_Mov(A_Reg(A_RegName.Arg1), A_Imm(ZERO_IMM), PTR_SIZE)
    program += A_Call(F_FLUSH)
    program += A_Mov(A_Reg(A_RegName.StackPtr), A_Reg(A_RegName.BasePtr), PTR_SIZE)
    program += A_Pop(A_Reg(A_RegName.BasePtr))
    program += A_Ret

    A_Func(PRINTLN_LABEL, program.toList)


inline def defaultReadc: A_Func = 
    defRead(CHAR_SIZE, READC_LBL_STR_NAME, READC_LABEL)

inline def defaultReadi: A_Func = 
    defRead(INT_SIZE, READI_LBL_STR_NAME, READI_LABEL)

inline def defRead(size: A_OperandSize, dataLabel: A_DataLabel, defLabel: A_DefaultLabel): A_Func = 
    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_Push(A_Reg(A_RegName.BasePtr))
    program += A_Mov(A_Reg(A_RegName.BasePtr), A_Reg(A_RegName.StackPtr), PTR_SIZE)
    program += A_And(A_Reg(A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Sub(A_Reg(A_RegName.StackPtr), A_Imm(2 * opSizeToInt(PTR_SIZE)), PTR_SIZE)
    program += A_Mov(A_RegDeref(A_MemOffset(A_Reg(A_RegName.StackPtr), A_OffsetImm(ZERO_IMM))), A_Reg(A_RegName.Arg1), size)
    program += A_Lea(A_Reg(A_RegName.Arg2), A_MemOffset(A_Reg(A_RegName.StackPtr), A_OffsetImm(ZERO_IMM)))
    program += A_Lea(A_Reg(A_RegName.Arg1), A_MemOffset(A_Reg(A_RegName.InstrPtr), A_OffsetLbl(dataLabel)))
    program += A_Mov(A_Reg(A_RegName.RetReg), A_Imm(ZERO_IMM), BYTE_SIZE)
    program += A_Call(SCANF)
    program += A_Mov(A_Reg(A_RegName.RetReg), A_RegDeref(A_MemOffset(A_Reg(A_RegName.StackPtr), A_OffsetImm(ZERO_IMM))), size)
    program += A_Add(A_Reg(A_RegName.StackPtr), A_Imm(2 * opSizeToInt(PTR_SIZE)), PTR_SIZE)
    program += A_Mov(A_Reg(A_RegName.StackPtr), A_Reg(A_RegName.BasePtr), PTR_SIZE)
    program += A_Pop(A_Reg(A_RegName.BasePtr))
    program += A_Ret

    A_Func(defLabel, program.toList)

inline def defaultBadChar: A_Func = 
    defRuntimeErr(ERR_BAD_CHAR_STR_NAME, ERR_BAD_CHAR_LABEL)

inline def defaultOutOfBounds: A_Func = 
    defRuntimeErr(OUT_OF_BOUNDS_LBL_STR_NAME, ERR_OUT_OF_BOUNDS_LABEL)

inline def defaultDivZero: A_Func = 
    defRuntimeErr(PRINTS_LABEL, DIV_ZERO_LBL_STR_NAME, ERR_DIV_ZERO_LABEL)

inline def defaultOutOfMemory: A_Func = 
    defRuntimeErr(PRINTS_LABEL, OUT_OF_MEMORY_LBL_STR_NAME, ERR_OUT_OF_MEMORY_LABEL)

inline def defaultErrNull: A_Func =
    defRuntimeErr(PRINTS_LABEL, ERR_NULL_PAIR_STR_NAME, ERR_NULL_PAIR_LABEL)

inline def defRuntimeErr(dataLabel: A_DataLabel, defLabel: A_DefaultLabel): A_Func = 
    val program: ListBuffer[A_Instr] = ListBuffer()
    
    program += A_And(A_Reg(A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Lea(A_Reg(A_RegName.Arg1), A_MemOffset(A_Reg(A_RegName.InstrPtr), A_OffsetLbl(dataLabel)))
    program += A_Mov(A_Reg(A_RegName.RetReg), A_Imm(ZERO_IMM), BYTE_SIZE)
    program += A_Call(PRINTF)

    /* Put 0 into the 64-bit R1 (rdi) to flush all output streams. Note: PTR_SIZE because first argument of fflush is a ptr */
    program += A_Mov(A_Reg(A_RegName.Arg1), A_Imm(ZERO_IMM), PTR_SIZE)
    program += A_Call(F_FLUSH)
    program += A_Mov(A_Reg(A_RegName.Arg1), A_Imm(ERR_EXIT_CODE), BYTE_SIZE)
    program += A_Call(EXIT)

    A_Func(defLabel, program.toList)

inline def defRuntimeErr(callLabel: A_DefaultLabel, dataLabel: A_DataLabel, defLabel: A_DefaultLabel) =
    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_And(A_Reg(A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Lea(A_Reg(A_RegName.Arg1), A_MemOffset(A_Reg(A_RegName.InstrPtr), A_OffsetLbl(dataLabel)))
    program += A_Call(callLabel)
    program += A_Mov(A_Reg(A_RegName.Arg1), A_Imm(ERR_EXIT_CODE), BYTE_SIZE)
    program += A_Call(EXIT)

    A_Func(defLabel, program.toList)

inline def defaultMalloc: A_Func = 
    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_Push(A_Reg(A_RegName.BasePtr))
    program += A_Mov(A_Reg(A_RegName.BasePtr), A_Reg(A_RegName.StackPtr), PTR_SIZE)
    program += A_And(A_Reg(A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Call(MALLOC)
    program += A_Cmp(A_Reg(A_RegName.RetReg), A_Imm(ZERO_IMM), PTR_SIZE)
    program += A_Jmp(ERR_OUT_OF_MEMORY_LABEL, A_Cond.Eq)
    program += A_Mov(A_Reg(A_RegName.StackPtr), A_Reg(A_RegName.BasePtr), PTR_SIZE)
    program += A_Pop(A_Reg(A_RegName.BasePtr))
    program += A_Ret

    A_Func(MALLOC_LABEL, program.toList)

inline def defaultFree: A_Func = 
    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_Push(A_Reg(A_RegName.BasePtr))
    program += A_Mov(A_Reg(A_RegName.BasePtr), A_Reg(A_RegName.StackPtr), PTR_SIZE)
    program += A_And(A_Reg(A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Call(FREE)
    program += A_Mov(A_Reg(A_RegName.StackPtr), A_Reg(A_RegName.BasePtr), PTR_SIZE)
    program += A_Pop(A_Reg(A_RegName.BasePtr))
    program += A_Ret

    A_Func(FREE_LABEL, program.toList)

inline def defaultFreePair: A_Func = 
    val program: ListBuffer[A_Instr] = ListBuffer()

    program += A_Push(A_Reg(A_RegName.BasePtr))
    program += A_Mov(A_Reg(A_RegName.BasePtr), A_Reg(A_RegName.StackPtr), PTR_SIZE)
    program += A_And(A_Reg(A_RegName.StackPtr), A_Imm(STACK_ALIGN_VAL), PTR_SIZE)
    program += A_Cmp(A_Reg(A_RegName.Arg1), A_Imm(ZERO_IMM), PTR_SIZE)
    program += A_Jmp(ERR_OUT_OF_MEMORY_LABEL, A_Cond.Eq)
    program += A_Call(FREE)
    program += A_Mov(A_Reg(A_RegName.StackPtr), A_Reg(A_RegName.BasePtr), PTR_SIZE)
    program += A_Pop(A_Reg(A_RegName.BasePtr))
    program += A_Ret

    A_Func(FREE_PAIR_LABEL, program.toList)
