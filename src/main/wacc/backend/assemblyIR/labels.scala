package wacc.assemblyIR

/**
 * A_StoredStr represents a string stored in the assembly IR with its associated label
 * @param lbl the label to be generated to identify the string
 * @param str the string to be stored
 */
case class A_StoredStr(lbl: A_DataLabel, str: String)

/**
  * A_Label represents a label in the assembly IR
  * @param name the name of the label
  */
sealed trait A_Label { val name: String }
case class A_DataLabel(val name: String) extends A_Label 
case class A_InstrLabel(val name: String) extends A_Label 
case class A_DefaultLabel(val name: String) extends A_Label
case class A_ExternalLabel(val name: String) extends A_Label

/**
  * Some more cheeeky macros for common labels
  */
inline def OVERFLOW_LBL_STR = "fatal error: integer overflow or underflow occurred"
inline def OVERFLOW_LBL_STR_NAME = A_DataLabel(".L._errOverflow_str")

inline def DIV_ZERO_LBL_STR = "fatal error: division or modulo by zero"
inline def DIV_ZERO_LBL_STR_NAME = A_DataLabel(".L._errDivZero_str")

inline def OUT_OF_BOUNDS_LBL_STR = "Error: Array index out of bounds"
inline def OUT_OF_BOUNDS_LBL_STR_NAME = A_DataLabel(".L._errOutOfBounds_str")

inline def OUT_OF_MEMORY_LBL_STR = "Error: Out of memory"
inline def OUT_OF_MEMORY_LBL_STR_NAME = A_DataLabel(".L._errOutOfMemory_str")

inline def PRINTLN_LBL_STR = ""
inline def PRINTLN_LBL_STR_NAME = A_DataLabel(".L._println_str")

inline def PRINTI_LBL_STR = "%d"
inline def PRINTI_LBL_STR_NAME = A_DataLabel(".L._printi_int")

inline def PRINTC_LBL_STR = "%c"
inline def PRINTC_LBL_STR_NAME = A_DataLabel(".L._printc_str")

inline def PRINTP_LBL_STR = "%p"
inline def PRINTP_LBL_STR_NAME = A_DataLabel(".L._printp_str")

inline def PRINTB_TRUE_LBL_STR = "true"
inline def PRINTB_TRUE_LBL_STR_NAME = A_DataLabel(".L._printb_str_true")

inline def PRINTB_FALSE_LBL_STR = "false"
inline def PRINTB_FALSE_LBL_STR_NAME = A_DataLabel(".L._printb_str_false")

inline def PRINTB_LBL_STR = "%.*s"
inline def PRINTB_LBL_STR_NAME = A_DataLabel(".L._printb_str")

inline def PRINTS_LBL_STR = "%.*s"
inline def PRINTS_LBL_STR_NAME = A_DataLabel(".L._prints_str")

inline def READI_LBL_STR = "%d"
inline def READI_LBL_STR_NAME = A_DataLabel(".L._readi_str")

inline def READC_LBL_STR = " %c"
inline def READC_LBL_STR_NAME = A_DataLabel(".L._readc_str")

inline def ERR_BAD_CHAR_STR = "fatal error: int %d is not ascii character 0-127"
inline def ERR_BAD_CHAR_STR_NAME = A_DataLabel(".L._errBadChar_str")

inline def ERR_NULL_PAIR_STR = "fatal error: null pair dereferenced or freed"
inline def ERR_NULL_PAIR_STR_NAME = A_DataLabel(".L._errNull_str")

inline def F_FLUSH = A_ExternalLabel("fflush")
inline def PUTS = A_ExternalLabel("puts")
inline def EXIT = A_ExternalLabel("exit")
inline def MALLOC = A_ExternalLabel("malloc")
inline def FREE = A_ExternalLabel("free")
inline def PRINTF = A_ExternalLabel("printf")
inline def SCANF = A_ExternalLabel("scanf")

inline def ERR_BAD_CHAR_LABEL = A_DefaultLabel("_errBadChar")
inline def ERR_OVERFLOW_LABEL = A_DefaultLabel("_errOverflow")
inline def ERR_OUT_OF_BOUNDS_LABEL = A_DefaultLabel("_errOutOfBounds")
inline def ERR_OUT_OF_MEMORY_LABEL = A_DefaultLabel("_errOutOfMemory")
inline def ERR_DIV_ZERO_LABEL = A_DefaultLabel("_errDivZero")
inline def ERR_NULL_PAIR_LABEL = A_DefaultLabel("_errNull")
inline def PRINTLN_LABEL = A_DefaultLabel("_println")
inline def PRINTI_LABEL = A_DefaultLabel("_printi")
inline def PRINTC_LABEL = A_DefaultLabel("_printc")
inline def PRINTP_LABEL = A_DefaultLabel("_printp")
inline def PRINTB_LABEL = A_DefaultLabel("_printb")
inline def PRINTS_LABEL = A_DefaultLabel("_prints")
inline def PRINTB_FALSE_LABEL = A_DefaultLabel("_printb_false")
inline def PRINTB_TRUE_LABEL = A_DefaultLabel("_printb_true")
inline def READI_LABEL = A_DefaultLabel("_readi")
inline def READC_LABEL = A_DefaultLabel("_readc")
inline def EXIT_LABEL = A_DefaultLabel("_exit")
inline def ARR_LD1_LABEL = A_DefaultLabel("_arrLoad1")
inline def ARR_LD4_LABEL = A_DefaultLabel("_arrLoad4")
inline def ARR_LD8_LABEL = A_DefaultLabel("_arrLoad8")
inline def MALLOC_LABEL = A_DefaultLabel("_malloc")
inline def FREE_LABEL = A_DefaultLabel("_free")
inline def FREE_PAIR_LABEL = A_DefaultLabel("_freePair")
