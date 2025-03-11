package wacc.codeGen

import wacc.t_ast.*
import wacc.q_ast.Name
import wacc.assemblyIR.*
import wacc.ast.PairIndex
import wacc.SemType
import wacc.?
import wacc.X
import wacc.KnownType

import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.collection.immutable

/**
 * mention edge cases: integer overflow, division by zero
 */ 
def genDivMod(x: T_Expr, y: T_Expr, divResultReg: A_RegName, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    // NOTE: With IDiv, we need the numerator to be stored in eax and then we can divide by a given register
    // NOTE: IDiv stores remainder in edx(32bit) - R3
    ctx.addDefaultFunc(ERR_DIV_ZERO_LABEL)
    ctx.addDefaultFunc(ERR_OVERFLOW_LABEL)

    builder ++= gen(x, stackTable)
    builder += A_Push(A_Reg(A_RegName.RetReg))
    builder ++= gen(y, stackTable)
    builder += A_Mov(A_Reg(A_RegName.Arg1), A_Reg(A_RegName.RetReg), INT_SIZE)
    builder += A_Cmp(A_Reg(A_RegName.Arg1), A_Imm(ZERO_IMM), INT_SIZE) // Compare denominator with 0
    builder += A_Jmp(ERR_DIV_ZERO_LABEL, A_Cond.Eq) // comparison of denominator with 0 - error if it succeeds
    builder += A_Pop(A_Reg(A_RegName.RetReg))
    builder += A_CDQ
    builder += A_IDiv(A_Reg(A_RegName.Arg1), INT_SIZE)
    builder += A_Jmp(ERR_OVERFLOW_LABEL, A_Cond.Overflow) // This is the case of dividing -2^31 by -1 and getting 2^31 > 1 + 2^31 --> overflow
    builder += A_Mov(A_Reg(A_RegName.RetReg), (A_Reg(divResultReg)), INT_SIZE)

    builder.toList

def genAddSub(x: T_Expr, y: T_Expr, instrApply: ((A_Reg, A_Operand, A_OperandSize) => A_Instr), stackTable: StackTables)(using ctx: CodeGenCtx) =
    val builder = new ListBuffer[A_Instr]
    
    ctx.addDefaultFunc(ERR_OVERFLOW_LABEL)

    builder ++= gen(x, stackTable)
    builder += A_Push(A_Reg(A_RegName.RetReg))
    builder ++= gen(y, stackTable)
    builder += A_Mov(A_Reg(A_RegName.Arg1), A_Reg(A_RegName.RetReg), INT_SIZE)
    builder += A_Pop(A_Reg(A_RegName.RetReg))
    builder += instrApply(A_Reg(A_RegName.RetReg), A_Reg(A_RegName.Arg1), INT_SIZE)
    builder += A_Jmp(ERR_OVERFLOW_LABEL, A_Cond.Overflow)

    builder.toList

def genMul(x: T_Expr, y: T_Expr, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = 
    
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x, stackTable)
    builder += A_Push(A_Reg(A_RegName.RetReg))
    builder ++= gen(y, stackTable)
    builder += A_Pop(A_Reg(A_RegName.Arg1))
    builder += A_IMul(A_Reg(A_RegName.RetReg), A_Reg(A_RegName.Arg1), INT_SIZE)

    ctx.addDefaultFunc(ERR_OVERFLOW_LABEL)

    builder += A_Jmp(ERR_OVERFLOW_LABEL, A_Cond.Overflow)

    builder.toList

def genComparison(x: T_Expr, y: T_Expr, ty: SemType, cond: A_Cond, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x, stackTable)
    
    val sizeTy = sizeOf(ty)

    builder += A_Push(A_Reg(A_RegName.RetReg))
    builder ++= gen(y, stackTable)
    builder += A_Pop(A_Reg(A_RegName.Arg1))
    builder += A_Cmp(A_Reg(A_RegName.Arg1), A_Reg(A_RegName.RetReg), sizeTy)
    builder += A_Set(A_Reg(A_RegName.RetReg), cond)

    builder.toList

def genBitwiseOp(x: T_Expr, y: T_Expr, stackTable: StackTables, cond: A_Cond)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]
    val label = ctx.genNextInstrLabel()

    builder ++= gen(x, stackTable)
    builder += A_Cmp(A_Reg(A_RegName.RetReg), A_Imm(TRUE), BOOL_SIZE)
    builder += A_Jmp(label, cond)
    builder ++= gen(y, stackTable)
    builder += A_Cmp(A_Reg(A_RegName.RetReg), A_Imm(TRUE), BOOL_SIZE)
    builder += A_LabelStart(label)
    builder += A_Set(A_Reg(A_RegName.RetReg), A_Cond.Eq)

    builder.toList

def genNot(x: T_Expr, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = 
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x, stackTable)
    builder += A_Xor(A_Reg(A_RegName.RetReg), A_Imm(TRUE), BOOL_SIZE)

    builder.toList

def genNeg(x: T_Expr, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =    
    val builder = new ListBuffer[A_Instr]

    ctx.addDefaultFunc(ERR_OVERFLOW_LABEL)

    builder ++= gen(x, stackTable)
    builder += A_Mov(A_Reg(A_RegName.Arg1), A_Imm(ZERO_IMM), INT_SIZE)
    builder += A_Sub(A_Reg(A_RegName.Arg1), A_Reg(A_RegName.RetReg), INT_SIZE)
    builder += A_Jmp(ERR_OVERFLOW_LABEL, A_Cond.Overflow)  // overflow -2^32 case!
    builder += A_Mov(A_Reg(A_RegName.RetReg), A_Reg(A_RegName.Arg1), INT_SIZE)
   
    builder.toList

def genLen(x: T_Expr, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    // PRE: WE KNOW x IS A LIST BECAUSE OF TYPE CHECKING
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x, stackTable)
    // We now have the pointer to the first element stored in RAX (64-bit RetReg)
    // We know the size is stored 4 bytes before the first element hence we can do a reg deref of retreg -4 to find the size
    builder += A_Mov(
        A_Reg(A_RegName.RetReg), 
        A_RegDeref(A_MemOffset(A_Reg(A_RegName.RetReg), A_OffsetImm(-numOfBytes(INT_SIZE)))), 
        INT_SIZE)

    builder.toList

def genOrd(x: T_Expr, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = 
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x, stackTable)
    builder += A_Movzx(A_Reg(A_RegName.Arg1), A_Reg(A_RegName.RetReg), INT_SIZE, CHAR_SIZE)

    builder.toList

def genChr(x: T_Expr, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = 
    val builder = new ListBuffer[A_Instr]

    ctx.addDefaultFunc(ERR_BAD_CHAR_LABEL)

    builder ++= gen(x, stackTable)
    builder += A_Mov(A_Reg(A_RegName.Arg1), A_Reg(A_RegName.RetReg), INT_SIZE)
    builder += A_And(A_Reg(A_RegName.Arg1), A_Imm(-128), INT_SIZE)    
    builder += A_Jmp(ERR_BAD_CHAR_LABEL, A_Cond.NEq)

    builder.toList

def genIntLiteral(v: BigInt)(using ctx: CodeGenCtx): List[A_Instr] = 
    List(A_Mov(A_Reg(A_RegName.RetReg), A_Imm(v), INT_SIZE))

def genBoolLiteral(v: Boolean)(using ctx: CodeGenCtx): List[A_Instr] = 
    List(A_Mov(A_Reg(A_RegName.RetReg), A_Imm(if v then TRUE else FALSE), BOOL_SIZE))

def genCharLiteral(v: Char)(using ctx: CodeGenCtx): List[A_Instr] = 
    List(A_Mov(A_Reg(A_RegName.RetReg), A_Imm(v.toInt), CHAR_SIZE))

def genStringLiteral(v: String)(using ctx: CodeGenCtx): List[A_Instr] = 
    val str = v.flatMap( _ match {
        case '\n' => "\\n"
        case '\t' => "\\t"
        case '\b' => "\\b"
        case '\r' => "\\r"
        case '\f' => "\\f"
        case '\\' => "\\\\"
        case '\"' => "\\\""
        case '\'' => "\\\'"
        case c => c.toString
    })
    
    List(A_Lea(A_Reg(A_RegName.RetReg), A_MemOffset(A_Reg(A_RegName.InstrPtr), A_OffsetLbl(ctx.storeString(str)))))

def genIdent(v: Name, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = 
    stackTable.get(v).toList

def unwrapArrType(ty: KnownType, length: Int): SemType = (ty, length) match
    case (_, 0) => ty
    case (wacc.KnownType.Array(t), _) => unwrapArrType(t.asInstanceOf[KnownType], length - 1)
    case _ => throw Exception(s"Received a type that isn't an array: $ty")

def getPointerToArrayElem(v: Name, indices: List[T_Expr], stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder: ListBuffer[A_Instr] = ListBuffer()
    val ty = unwrapArrType(ctx.typeInfo.varTys(v), indices.length)

    builder ++= genIdent(v, stackTable)

    for i <- 0 to indices.length - 2 do
        builder += A_Push(A_Reg(A_RegName.RetReg))
        builder += A_Mov(A_Reg(A_RegName.RetReg), A_Imm(ZERO_IMM), PTR_SIZE)
        builder ++= gen(indices(i), stackTable)
        builder ++= indexArray(numOfBytes(PTR_SIZE))
        builder += A_Mov(A_Reg(A_RegName.RetReg), A_RegDeref(A_MemOffset(A_Reg(A_RegName.RetReg), A_OffsetImm(0))), PTR_SIZE)
    
    builder += A_Push(A_Reg(A_RegName.RetReg))
    builder += A_Mov(A_Reg(A_RegName.RetReg), A_Imm(ZERO_IMM), PTR_SIZE)
    builder ++= gen(indices(indices.length - 1), stackTable)
    builder ++= indexArray(numOfBytes(sizeOf(ty)))

    builder.toList

// assumes index is stored in RetReg and that the pointer to the array is pushed onto the stack
// stores the address of the indexed element in RetReg
def indexArray(elemSize: Int)(using ctx: CodeGenCtx) =
    val builder: ListBuffer[A_Instr] = ListBuffer()

    ctx.addDefaultFunc(ERR_OUT_OF_BOUNDS_LABEL)
    
    builder += A_Cmp(A_Reg(A_RegName.RetReg), A_Imm(ZERO_IMM), INT_SIZE) // check i > 0
    builder += A_Jmp(ERR_OUT_OF_BOUNDS_LABEL, A_Cond.Lt)
    builder += A_Pop(A_Reg(A_RegName.Arg1)) // retrieve array size
    builder += A_Mov(A_Reg(A_RegName.Arg2), A_RegDeref(A_MemOffset(A_Reg(A_RegName.Arg1), A_OffsetImm(-numOfBytes(INT_SIZE)))), INT_SIZE)
    builder += A_Push(A_Reg(A_RegName.Arg1))
    builder += A_Cmp(A_Reg(A_RegName.RetReg), A_Reg(A_RegName.Arg2), INT_SIZE)  // check i < size
    builder += A_Jmp(ERR_OUT_OF_BOUNDS_LABEL, A_Cond.GEq)
    builder += A_IMul(A_Reg(A_RegName.RetReg), A_Imm(elemSize), PTR_SIZE) // calculate offset
    builder += A_Pop(A_Reg(A_RegName.Arg1))
    builder += A_Add(A_Reg(A_RegName.RetReg), A_Reg(A_RegName.Arg1), PTR_SIZE)

    builder.toList

def genArrayElem(v: Name, indices: List[T_Expr], stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder: ListBuffer[A_Instr] = ListBuffer()
    val ty = unwrapArrType(ctx.typeInfo.varTys(v), indices.length)

    builder ++= getPointerToArrayElem(v, indices, stackTable)
    builder += A_Mov(A_Reg(A_RegName.RetReg), A_RegDeref(A_MemOffset(A_Reg(A_RegName.RetReg), NO_OFFSET)), sizeOf(ty))

    builder.toList

def genPairNullLiteral()(using ctx: CodeGenCtx): List[A_Instr] = 
    List(A_Mov(A_Reg(A_RegName.RetReg), A_Imm(ZERO_IMM), PTR_SIZE))

def getArrInnerType(ty: SemType): SemType = ty match
    case KnownType.Array(t) => getArrInnerType(t)
    case t => t

def getPairElemPtr(index: PairIndex, v: T_LValue, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]
    val offset = index match
        case PairIndex.First => ZERO_IMM
        case PairIndex.Second => numOfBytes(PTR_SIZE)

    ctx.addDefaultFunc(ERR_NULL_PAIR_LABEL)

    builder ++= gen(v, stackTable)
    builder += A_Cmp(A_Reg(A_RegName.RetReg), A_Imm(ZERO_IMM), PTR_SIZE)
    builder += A_Jmp(ERR_NULL_PAIR_LABEL, A_Cond.Eq)
    builder += A_Add(A_Reg(A_RegName.RetReg), A_Imm(offset), PTR_SIZE)

    builder.toList

def genPairElem(index: PairIndex, v: T_LValue, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    builder ++= getPairElemPtr(index, v, stackTable) // Now we have the pointer to fst/snd p in RetReg

    v match
        case T_Ident(name) =>
            // We assume pointer to our elem is stored in RetReg
            // deref this value to get value stored
            val pairTy = ctx.typeInfo.varTys(name).asInstanceOf[KnownType.Pair]
            val ty = index match
                case PairIndex.First => pairTy.ty1
                case PairIndex.Second => pairTy.ty2

            builder += A_Mov(A_Reg(A_RegName.RetReg), A_RegDeref(A_MemOffset(A_Reg(A_RegName.RetReg), NO_OFFSET)), sizeOf(ty))
        case _ =>
            // Either T_ArrayElem or T_PairElem
            // we assume the value in RetReg is a pointer to the element and deref this value to get value stored
            builder += A_Mov(A_Reg(A_RegName.RetReg), A_RegDeref(A_MemOffset(A_Reg(A_RegName.RetReg), NO_OFFSET)), PTR_SIZE)

    builder.toList



def genArrayLiteral(xs: List[T_Expr], ty: SemType, length: Int, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]
    val sizeBytes = numOfBytes(INT_SIZE) + (numOfBytes(ty) * length)

    ctx.addDefaultFunc(MALLOC_LABEL)

    builder += A_Mov(A_Reg(A_RegName.Arg1), A_Imm(sizeBytes), INT_SIZE)
    builder += A_Call(MALLOC_LABEL)
    builder += A_Mov(TEMP_REG, A_Reg(A_RegName.RetReg), PTR_SIZE)
    builder += A_Add(TEMP_REG, A_Imm(numOfBytes(INT_SIZE)), PTR_SIZE)
    builder += A_Mov(A_Reg(A_RegName.RetReg), A_Imm(length), INT_SIZE)
    builder += A_Mov(A_RegDeref(A_MemOffset(TEMP_REG, A_OffsetImm(-numOfBytes(INT_SIZE)))), A_Reg(A_RegName.RetReg), INT_SIZE)

    for i <- 0 to length - 1 do
        builder ++= gen(xs(i), stackTable)
        builder += A_Mov(A_RegDeref(A_MemOffset(TEMP_REG, A_OffsetImm(i * numOfBytes(ty)))), A_Reg(A_RegName.RetReg), sizeOf(ty))

    builder += A_Mov(A_Reg(A_RegName.RetReg), TEMP_REG, PTR_SIZE)

    builder.toList

def genNewPair(x1: T_Expr, x2: T_Expr, ty1: SemType, ty2: SemType, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    ctx.addDefaultFunc(MALLOC_LABEL)

    builder += A_Mov(A_Reg(A_RegName.Arg1), A_Imm(numOfBytes(PTR_SIZE) * 2), INT_SIZE)
    builder += A_Call(MALLOC_LABEL)
    builder += A_Mov(TEMP_REG, A_Reg(A_RegName.RetReg), PTR_SIZE)
    builder ++= gen(x1, stackTable)
    builder += A_Mov(A_RegDeref(A_MemOffset(TEMP_REG, NO_OFFSET)), A_Reg(A_RegName.RetReg), sizeOf(ty1))
    builder ++= gen(x2, stackTable)
    builder += A_Mov(A_RegDeref(A_MemOffset(TEMP_REG, A_OffsetImm(PAIR_OFFSET_SIZE))), A_Reg(A_RegName.RetReg), sizeOf(ty2))
    builder += A_Mov(A_Reg(A_RegName.RetReg), TEMP_REG, PTR_SIZE)

    builder.toList