package wacc.codeGen

import wacc.t_ast.*
import wacc.q_ast.Name
import wacc.assemblyIR.*
import wacc.ast.PairIndex
import wacc.semantic.SemType
import wacc.semantic.?
import wacc.semantic.X
import wacc.semantic.KnownType

import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.collection.immutable

/**
 * Generates the assembly instructions for a Div/Mod operation.
 * Handles the edge cases of division by zero and integer overflow.
 * With IDiv, we need the numerator to be stored in eax and then we can divide by a given register.
 * IDiv stores remainder in edx(32bit) ie R3. So we use this function for both div and mod.
 * 
 * @param x The numerator expression.
 * @param y The denominator expression.
 * @param divResultReg The register to store the result of the division.
 * @param stackTable The stack table.
 * @return The list of assembly instructions.
 */ 
def genDivMod(x: T_Expr, y: T_Expr, divResultReg: A_RegName, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

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

/**
  * Generates the assembly instructions for an Add/Sub operation.
  * @param x The first expression.
  * @param y The second expression.
  * @param instrApply The instruction to apply (Add or Sub).
  * @param stackTable The stack table.
  * @return The list of assembly instructions.
  */
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

/**
  * Generates the assembly instructions for a Mul operation.
  * @param x The first expression.
  * @param y The second expression.
  * @param stackTable The stack table.
  * @return The list of assembly instructions.
  */
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

/**
  * Generates the assembly instructions for a comparison.
  * @param x The first expression.
  * @param y The second expression.
  * @param ty The type of the expressions.
  * @param cond The condition to check.
  * @param stackTable The stack table.
  * @return The list of assembly instructions.
  */
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

/**
  * Generates the assembly instructions for a bitwise operation.
  * @param x The first expression.
  * @param y The second expression.
  * @param stackTable The stack table.
  * @param cond The condition to check.
  * @return The list of assembly instructions.
  */
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

/**
  * Generates the assembly instructions for a Not operation.
  * @param x The expression to generate code for.
  * @param stackTable The stack table.
  * @return The list of assembly instructions.
  */
def genNot(x: T_Expr, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = 
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x, stackTable)
    builder += A_Xor(A_Reg(A_RegName.RetReg), A_Imm(TRUE), BOOL_SIZE)

    builder.toList

/**
  * Generates the assembly instructions for a Negation operation.
  * @param x The expression to generate code for.
  * @param stackTable The stack table.
  * @return The list of assembly instructions.
  */
def genNeg(x: T_Expr, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =    
    val builder = new ListBuffer[A_Instr]

    ctx.addDefaultFunc(ERR_OVERFLOW_LABEL)

    builder ++= gen(x, stackTable)
    builder += A_Mov(A_Reg(A_RegName.Arg1), A_Imm(ZERO_IMM), INT_SIZE)
    builder += A_Sub(A_Reg(A_RegName.Arg1), A_Reg(A_RegName.RetReg), INT_SIZE)
    builder += A_Jmp(ERR_OVERFLOW_LABEL, A_Cond.Overflow)  // overflow -2^32 case!
    builder += A_Mov(A_Reg(A_RegName.RetReg), A_Reg(A_RegName.Arg1), INT_SIZE)
   
    builder.toList

/**
  * Generates the assembly instructions for a Len operation.
  * @param x The expression to generate code for.
  * @param stackTable The stack table.
  * @return The list of assembly instructions.
  */
def genLen(x: T_Expr, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    // PRE: WE KNOW x IS A LIST BECAUSE OF TYPE CHECKING
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x, stackTable)
    /* We now have the pointer to the first element stored in RAX (64-bit RetReg)
    We know the size is stored 4 bytes before the first element hence we can do a reg deref of retreg -4 to find the size */
    builder += A_Mov(
        A_Reg(A_RegName.RetReg), 
        A_RegDeref(A_MemOffset(A_Reg(A_RegName.RetReg), A_OffsetImm(-numOfBytes(INT_SIZE)))), 
        INT_SIZE)

    builder.toList

/**
  * Generates the assembly instructions for a Ord operation (char -> int).
  * @param x The expression to generate code for.
  * @param stackTable The stack table.
  * @return The list of assembly instructions.
  */
def genOrd(x: T_Expr, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = 
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x, stackTable)
    builder += A_Movzx(A_Reg(A_RegName.Arg1), A_Reg(A_RegName.RetReg), INT_SIZE, CHAR_SIZE)

    builder.toList

/**
  * Generates the assembly instructions for a Ord operation (int -> char).
  * @param x The expression to generate code for.
  * @param stackTable The stack table.
  * @return The list of assembly instructions.
  */
def genChr(x: T_Expr, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = 
    val builder = new ListBuffer[A_Instr]

    ctx.addDefaultFunc(ERR_BAD_CHAR_LABEL)

    builder ++= gen(x, stackTable)
    builder += A_Mov(A_Reg(A_RegName.Arg1), A_Reg(A_RegName.RetReg), INT_SIZE)
    builder += A_And(A_Reg(A_RegName.Arg1), A_Imm(-128), INT_SIZE)    
    builder += A_Jmp(ERR_BAD_CHAR_LABEL, A_Cond.NEq)

    builder.toList

/**
  * Generates an Int literal.
  * @param v the value of the literal.
  * @return The list of assembly instructions.
  */
def genIntLiteral(v: BigInt)(using ctx: CodeGenCtx): List[A_Instr] = 
    List(A_Mov(A_Reg(A_RegName.RetReg), A_Imm(v), INT_SIZE))

/**
  * Generates a Boolean literal.
  * @param v the value of the literal.
  * @return The list of assembly instructions.
  */
def genBoolLiteral(v: Boolean)(using ctx: CodeGenCtx): List[A_Instr] = 
    List(A_Mov(A_Reg(A_RegName.RetReg), A_Imm(if v then TRUE else FALSE), BOOL_SIZE))

/**
  * Generates a Char literal.
  * @param v the value of the literal.
  * @return The list of assembly instructions.
  */
def genCharLiteral(v: Char)(using ctx: CodeGenCtx): List[A_Instr] = 
    List(A_Mov(A_Reg(A_RegName.RetReg), A_Imm(v.toInt), CHAR_SIZE))

/**
  * Generates a String literal.
  * @param v the value of the literal.
  * @return The list of assembly instructions.
  */
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

/**
  * Generates the assembly instructions for an identifier.
  * @param v the name of the identifier.
  * @param stackTable The stack table.
  * @return The list of assembly instructions.
  */
def genIdent(v: Name, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = 
    stackTable.get(v).toList

/**
  * Generates the assembly instructions for extracting an array element.
  * @param v the name of the array.
  * @param indices the indices of the array element.
  * @param stackTable The stack table.
  * @return The list of assembly instructions.
  */
def genArrayElem(v: Name, indices: List[T_Expr], stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder: ListBuffer[A_Instr] = ListBuffer()
    val ty = unwrapArrType(ctx.typeInfo.varTys(v), indices.length)

    builder ++= getPointerToArrayElem(v, indices, stackTable)
    builder += A_Mov(A_Reg(A_RegName.RetReg), A_RegDeref(A_MemOffset(A_Reg(A_RegName.RetReg), NO_OFFSET)), sizeOf(ty))

    builder.toList

/**
  * Generates the assembly instructions for extracting a pointer to an array element.
  * @param v the name of the array.
  * @param indices the nested indices of the array element.
  * @param stackTable The stack table.
  * @return The list of assembly instructions.
  */
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

/**
  * Generates the assembly instructions for indexing an array.
  * Assumes index is stored in RetReg and that the pointer to the array is pushed onto the stack.
  * Stores the address of the indexed element in RetReg.
  * @param elemSize The size of the array element.
  * @return The list of assembly instructions.
  */
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

/**
  * Generates the assembly instructions for an array literal.
  * Stores all the given expressions in a 'malloced' array.
  * @param xs the list of expressions to store in the array.
  * @param ty the type of the array.
  * @param length the length of the array.
  * @param stackTable The stack table.
  * @return The list of assembly instructions.
  */
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

/**
  * Helper function to extract the inner type of a nested array.
  * @param ty The outside type of the array.
  * @param length The length of the 'indicies' vector (how many layers of nested arrays we have left).
  * @return The list of assembly instructions.
  */
def unwrapArrType(ty: KnownType, length: Int): SemType = (ty, length) match
    case (_, 0) => ty
    case (KnownType.Array(t), _) => unwrapArrType(t.asInstanceOf[KnownType], length - 1)
    case _ => throw Exception(s"Received a type that isn't an array: $ty")

/**
  * Generates the assembly instructions for a nullpair literal.
  * @return The list of assembly instructions.
  */
def genPairNullLiteral()(using ctx: CodeGenCtx): List[A_Instr] = 
    List(A_Mov(A_Reg(A_RegName.RetReg), A_Imm(ZERO_IMM), PTR_SIZE))

/**
  * Generates the assembly instructions for extracting a pair element.
  * @param index The index of the element (fst/snd).
  * @param v the pair, a [T_LValue] the evaluates to a pair.
  * @param stackTable The stack table.
  * @return The list of assembly instructions.
  */
def genPairElem(index: PairIndex, v: T_LValue, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    builder ++= getPairElemPtr(index, v, stackTable) // Now we have the pointer to fst/snd p in RetReg

    v match
        case T_Ident(name) =>
            /* We assume pointer to our elem is stored in RetReg
            deref this value to get value stored */
            val pairTy = ctx.typeInfo.varTys(name).asInstanceOf[KnownType.Pair]
            val ty = index match
                case PairIndex.First => pairTy.ty1
                case PairIndex.Second => pairTy.ty2

            builder += A_Mov(A_Reg(A_RegName.RetReg), A_RegDeref(A_MemOffset(A_Reg(A_RegName.RetReg), NO_OFFSET)), sizeOf(ty))
        case _ =>
            /* Either T_ArrayElem or T_PairElem
            we assume the value in RetReg is a pointer to the element and deref this value to get value stored */
            builder += A_Mov(A_Reg(A_RegName.RetReg), A_RegDeref(A_MemOffset(A_Reg(A_RegName.RetReg), NO_OFFSET)), PTR_SIZE)

    builder.toList

/**
  * helper function to get the pointer to a pair elem.
  * @param index The index of the element (fst/snd).
  * @param v the pair, a [T_LValue] the evaluates to a pair.
  * @param stackTable The stack table.
  * @return The list of assembly instructions.
  */
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

/**
  * Generates the assembly instructions for a newpair operation.
  * @param x1 The first expression.
  * @param x2 The second expression.
  * @param ty1 The type of the first expression.
  * @param ty2 The type of the second expression.
  * @param stackTable The stack table.
  * @return The list of assembly instructions.
  */
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
