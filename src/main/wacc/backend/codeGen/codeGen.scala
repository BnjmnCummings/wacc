package wacc.codeGen

import wacc.t_ast.*
import wacc.assemblyIR.*
import wacc.TypeInfo
import wacc.ast.PairIndex

import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import wacc.SemType
import wacc.?
import wacc.X
import wacc.KnownType

val TRUE = 1
val FALSE = 0
val ZERO_IMM = 0
val CHR_MASK = -128
val PAIR_SIZE_BYTES = 16
val PAIR_OFFSET_SIZE = 8
val INT_SIZE_BYTES = 4

def gen(t_tree: T_Prog, typeInfo: TypeInfo): A_Prog = {
    given ctx: CodeGenCtx = CodeGenCtx()

    val _funcs = t_tree.funcs.map(gen)
    val main = A_Func(A_InstrLabel("main"), t_tree.body.flatMap(gen))
    ???
}

private def gen(t: T_Stmt)(using ctx: CodeGenCtx): List[A_Instr] = t match
    case T_Decl(v, r, ty) => genDecl(v, r, ty)
    case T_Asgn(l, r, ty) => genAsgn(l, r, ty)
    case T_Read(l, ty) => genRead(l, ty)
    case T_Free(x, ty) => genFree(x, ty)
    case T_Return(x, ty) => genReturn(x, ty)
    case T_Exit(x) => genExit(x)
    case T_Print(x, ty) => genPrint(x, ty)
    case T_Println(x, ty) => genPrintln(x, ty)
    case T_If(cond, body, scopedBody, el, scopedEl) => genIf(cond, body, scopedBody, el, scopedEl)
    case T_While(cond, body, scoped) => genWhile(cond, body, scoped)
    case T_CodeBlock(body, scoped) => genCodeBlock(body, scoped)
    case T_Skip() => genSkip()

private def gen(t: T_Expr)(using ctx: CodeGenCtx): List[A_Instr] = t match
    case T_Mul(x, y) => genMul(x, y)
    case T_Div(x, y) => genDivMod(x, y, A_RegName.RetReg)
    case T_Mod(x, y) => genDivMod(x, y, A_RegName.R3)
    case T_Add(x, y) => genAddSub(x, y, A_Add.apply)
    case T_Sub(x, y) => genAddSub(x, y, A_Sub.apply)
    case T_GreaterThan(x, y, ty) => genComparison(x, y, ty, A_Cond.Gt)
    case T_GreaterThanEq(x, y, ty) => genComparison(x, y, ty, A_Cond.GEq)
    case T_LessThan(x, y, ty) => genComparison(x, y, ty, A_Cond.Lt)
    case T_LessThanEq(x, y, ty) => genComparison(x, y, ty, A_Cond.LEq)
    case T_Eq(x, y, ty) => genComparison(x, y, ty, A_Cond.Eq)
    case T_NotEq(x, y, ty) => genComparison(x, y, ty, A_Cond.NEq)
    case T_And(x, y) => genBitwiseOp(x, y, A_And.apply)
    case T_Or(x, y) => genBitwiseOp(x, y, A_Or.apply)
    case T_Not(x) => genNot(x)
    case T_Neg(x) => genNeg(x)
    case T_Len(x) => genLen(x)
    case T_Ord(x) => genOrd(x)
    case T_Chr(x) => genChr(x)
    case T_IntLiteral(v) => genIntLiteral(v)
    case T_BoolLiteral(v) => genBoolLiteral(v)
    case T_CharLiteral(v) => genCharLiteral(v)
    case T_StringLiteral(v) => genStringLiteral(v)
    case T_Ident(v) => genIdent(v)
    case T_ArrayElem(v, indices) => genArrayElem(v, indices)
    case T_PairNullLiteral => genPairNullLiteral()
    case T_PairElem(index, v) => genPairElem(index, v)

private def gen(t: T_LValue)(using ctx: CodeGenCtx) = t match
    case T_Ident(v) => genIdent(v)
    case T_ArrayElem(v, indices) => genArrayElem(v, indices)
    case T_PairElem(index, v) => genPairElem(index, v)

private def gen(t: T_RValue)(using ctx: CodeGenCtx) = t match
    case T_FuncCall(v, args) => genFuncCall(v, args)
    case T_ArrayLiteral(xs, ty, length) => genArrayLiteral(xs, ty, length)
    case T_NewPair(x1, x2, ty1, ty2) => genNewPair(x1, x2, ty1, ty2)

private def gen(t: T_Func)(using ctx: CodeGenCtx): A_Func = ???

private def genDecl(v: T_Name, r: T_RValue, ty: SemType)(using ctx: CodeGenCtx): List[A_Instr] = ???

private def genAsgn(l: T_LValue, r: T_RValue, ty: SemType)(using ctx: CodeGenCtx): List[A_Instr] = ???

private def genRead(l: T_LValue, ty: SemType)(using ctx: CodeGenCtx): List[A_Instr] = ???

private def genFree(x: T_Expr, ty: SemType)(using ctx: CodeGenCtx): List[A_Instr] = ???

private def genReturn(x: T_Expr, ty: SemType)(using ctx: CodeGenCtx): List[A_Instr] = ???

private def genExit(x: T_Expr)(using ctx: CodeGenCtx): List[A_Instr] = ???

private def genPrint(x: T_Expr, ty: SemType)(using ctx: CodeGenCtx): List[A_Instr] = ???

private def genPrintln(x: T_Expr, ty: SemType)(using ctx: CodeGenCtx): List[A_Instr] = ???

private def genIfHelper(cond: T_Expr, body: List[T_Stmt], el: List[T_Stmt])(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    val bodyLabel = ctx.genNextInstrLabel() // .L0
    val restLabel = ctx.genNextInstrLabel() // .L1

    builder ++= gen(cond)
    builder += A_Cmp(A_Reg(boolSize, A_RegName.RetReg), A_Imm(TRUE), boolSize)
    builder += A_Jmp(bodyLabel, A_Cond.Eq)

    el.foreach(builder ++= gen(_))

    builder += A_Jmp(restLabel, A_Cond.Uncond) 

    builder += A_LabelStart(bodyLabel)
    body.foreach(builder ++= gen(_))
    builder += A_LabelStart(restLabel)

    builder.toList

private def genIf(cond: T_Expr, body: List[T_Stmt], scopedBody: Set[T_Name], el: List[T_Stmt], scopedEl: Set[T_Name])(using ctx: CodeGenCtx): List[A_Instr] = 
    val builder = new ListBuffer[A_Instr]

    cond match
        case T_GreaterThan(x, y, ty) => builder ++= genIfHelper(cond, body, el)
        case T_GreaterThanEq(x, y, ty) => builder ++= genIfHelper(cond, body, el)
        case T_LessThan(x, y, ty) => builder ++= genIfHelper(cond, body, el)
        case T_LessThanEq(x, y, ty) => builder ++= genIfHelper(cond, body, el)
        case T_Eq(x, y, ty) => builder ++= genIfHelper(cond, body, el)
        case T_NotEq(x, y, ty) => builder ++= genIfHelper(cond, body, el)
        case T_And(x, y) => builder ++= genIfHelper(cond, body, el)
        case T_Or(x, y) => builder ++= genIfHelper(cond, body, el)
        case T_Not(x) => builder ++= genIfHelper(cond, body, el)
        case T_BoolLiteral(v) => builder ++= genIfHelper(cond, body, el)
        case T_Ident(v) => ???
        case _ => throw Exception(s"Should not reach here. Got $cond")

    builder.toList

private def genWhile(cond: T_Expr, body: List[T_Stmt], scoped: Set[T_Name])(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    val condLabel = ctx.genNextInstrLabel() // .L0
    val bodyLabel = ctx.genNextInstrLabel() // .L1

    builder += A_Jmp(condLabel, A_Cond.Uncond)
    
    builder += A_LabelStart(bodyLabel)
    body.foreach(builder ++= gen(_)) 

    builder += A_LabelStart(condLabel)
    builder ++= gen(cond)
    builder += A_Cmp(A_Reg(boolSize, A_RegName.RetReg), A_Imm(TRUE), boolSize)
    builder += A_Jmp(bodyLabel, A_Cond.Eq)

    builder.toList

private def genCodeBlock(body: List[T_Stmt], scoped: Set[T_Name])(using ctx: CodeGenCtx): List[A_Instr] = ???

private def genSkip(): List[A_Instr] = List()

private def genDivMod(x: T_Expr, y: T_Expr, divResultReg: A_RegName)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    // Note: With IDiv, we need the numerator to be stored in eax and then we can divide by a given register
    // Note: IDiv stores remainder in edx(32bit) - R3

    builder ++= gen(x)
    builder += A_Push(A_Reg(intSize, A_RegName.RetReg))
    builder ++= gen(y)
    builder += A_Mov(A_Reg(intSize, A_RegName.RetReg), A_Reg(intSize, A_RegName.R1))

    // Compare denominator with 0
    builder += A_Cmp(A_Reg(intSize, A_RegName.R1), A_Imm(0), intSize)
    builder += A_Jmp(???, A_Cond.Eq)
    // Above is a comparison of y (denominator) with 0
    // TODO @Aidan: Add a divide by 0 flag + label

    builder += A_Pop(A_Reg(intSize, A_RegName.RetReg))
    builder += A_IDiv(A_Reg(intSize, A_RegName.R1), intSize)
    // TODO @Aidan: Overflow can occur here - add flag system etc.
    // ^ This is the case of dividing -2^31 by -1 and getting 2^31 > 1 + 2^31 --> overflow

    builder += A_Mov(A_Reg(intSize, A_RegName.RetReg), (A_Reg(intSize, divResultReg)))

    builder.toList

private def genAddSub(x: T_Expr, y: T_Expr, instrApply: ((A_Reg, A_Operand, A_OperandSize) => A_Instr))(using ctx: CodeGenCtx) =
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x)
    builder += A_Push(A_Reg(intSize, A_RegName.RetReg))
    builder ++= gen(y)
    builder += A_Pop(A_Reg(intSize, A_RegName.R1))
    builder += instrApply(A_Reg(intSize, A_RegName.RetReg), A_Reg(intSize, A_RegName.R1), intSize)
    // TODO @Aidan: Overflow can occur here - add flag system etc.

    builder.toList

private def genMul(x: T_Expr, y: T_Expr)(using ctx: CodeGenCtx): List[A_Instr] = 
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x)
    builder += A_Push(A_Reg(intSize, A_RegName.RetReg))
    builder ++= gen(y)
    builder += A_Pop(A_Reg(intSize, A_RegName.R1))
    builder += A_IMul(A_Reg(intSize, A_RegName.RetReg), A_Reg(intSize, A_RegName.RetReg), A_Reg(intSize, A_RegName.R1), intSize)
    // TODO @Aidan: Overflow can occur here - add flag system etc.

    builder.toList

private def genComparison(x: T_Expr, y: T_Expr, ty: SemType, cond: A_Cond)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x)
    
    val sizeTy = sizeOf(ty)

    builder += A_Push(A_Reg(sizeTy, A_RegName.RetReg))
    builder ++= gen(y)
    builder += A_Pop(A_Reg(sizeTy, A_RegName.R1))
    builder += A_Cmp(A_Reg(sizeTy, A_RegName.R1), A_Reg(sizeTy, A_RegName.RetReg), sizeTy)
    builder += A_Set(A_Reg(boolSize, A_RegName.RetReg), cond)

    builder.toList

private def genBitwiseOp(x: T_Expr, y: T_Expr, instrApply: ((A_Reg, A_Operand, A_OperandSize) => A_Instr))(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x)
    builder += A_Push(A_Reg(boolSize, A_RegName.RetReg))
    builder ++= gen(y)
    builder += A_Pop(A_Reg(boolSize, A_RegName.R1))
    builder += instrApply(A_Reg(boolSize, A_RegName.RetReg), A_Reg(boolSize, A_RegName.R1), boolSize)

    builder.toList

private def genNot(x: T_Expr)(using ctx: CodeGenCtx): List[A_Instr] = 
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x)
    builder += A_Xor(A_Reg(boolSize, A_RegName.RetReg), A_Imm(TRUE), boolSize)

    builder.toList

private def genNeg(x: T_Expr)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x)

    // CONSIDER: DO WE NEED TO SAVE R1 BEFORE THIS?
    builder += A_Mov(A_Reg(intSize, A_RegName.R1), A_Imm(ZERO_IMM))
    builder += A_Sub(A_Reg(intSize, A_RegName.R1), A_Reg(intSize, A_RegName.RetReg), intSize)
    // check overflow -2^32 case! TODO @Aidan

    builder.toList

private def genLen(x: T_Expr)(using ctx: CodeGenCtx): List[A_Instr] = ???

private def genOrd(x: T_Expr)(using ctx: CodeGenCtx): List[A_Instr] = 
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x)
    builder += A_Movzx(A_Reg(intSize, A_RegName.R1), A_Reg(charSize, A_RegName.RetReg))

    builder.toList

private def genChr(x: T_Expr)(using ctx: CodeGenCtx): List[A_Instr] = 
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x)

    builder += A_Mov(A_Reg(intSize, A_RegName.R1), A_Reg(intSize, A_RegName.RetReg))
    builder += A_And(A_Reg(intSize, A_RegName.R1), A_Imm(CHR_MASK), intSize)
    builder += A_Cmp(A_Reg(intSize, A_RegName.R1), A_Imm(ZERO_IMM), intSize)
    builder += A_Jmp(???, A_Cond.NEq)
    // TODO: @Aidan Create bad character label - this is when you chr(x) |x| > 127 (0b1111111)

    builder.toList

private def genIntLiteral(v: BigInt)(using ctx: CodeGenCtx): List[A_Instr] = List(A_Mov(A_Reg(intSize, A_RegName.RetReg), A_Imm(v)))

private def genBoolLiteral(v: Boolean)(using ctx: CodeGenCtx): List[A_Instr] = List(A_Mov(A_Reg(boolSize, A_RegName.RetReg), A_Imm(if v then TRUE else FALSE)))

private def genCharLiteral(v: Char)(using ctx: CodeGenCtx): List[A_Instr] = List(A_Mov(A_Reg(charSize, A_RegName.RetReg), A_Imm(v.toInt)))

private def genStringLiteral(v: String)(using ctx: CodeGenCtx): List[A_Instr] = {
    val lbl = ctx.genStoredStr(v)

    val offset = A_MemOffset(ptrSize, A_Reg(A_OperandSize.A_64, A_RegName.InstrPtr), A_OffsetLbl(lbl))

    List(A_Lea(A_Reg(ptrSize, A_RegName.RetReg), offset))
}

private def genIdent(v: T_Name)(using ctx: CodeGenCtx): List[A_Instr] = ???

private def genArrayElem(v: T_Name, indices: List[T_Expr])(using ctx: CodeGenCtx): List[A_Instr] = ???

private def genPairNullLiteral()(using ctx: CodeGenCtx): List[A_Instr] = ???

private def genPairElem(index: PairIndex, v: T_LValue)(using ctx: CodeGenCtx): List[A_Instr] = ???

private def genFuncCall(v: T_Name, args: List[T_Expr])(using ctx: CodeGenCtx): List[A_Instr] = ???

private def genArrayLiteral(xs: List[T_Expr], ty: SemType, length: BigInt)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]
    val sizeBytes = INT_SIZE_BYTES + (sizeOfBytes(ty) * length)

    builder += A_Mov(A_Reg(A_OperandSize.A_32, A_RegName.R1), A_Imm(sizeBytes))
    builder += A_Call(A_ExternalLabel("malloc"))
    builder += A_Mov(A_Reg(A_OperandSize.A_64, A_RegName.R11), A_Reg(A_OperandSize.A_64, A_RegName.RetReg))
    builder += A_Add(A_Reg(A_OperandSize.A_64, A_RegName.R11), A_Imm(INT_SIZE_BYTES), intSize)
    builder += A_MovDeref(A_RegDeref(sizeOf(ty), A_MemOffset(sizeOf(ty), A_Reg(sizeOf(ty), A_RegName.R11), A_OffsetImm(-INT_SIZE_BYTES))), A_Imm(length))

    for (i <- 0 to length.asInstanceOf[Int]) { // TODO: as instance of used! haha (ranges won't take BigInt - refactor or leave?)
        builder ++= gen(xs(i))
        builder += A_MovDeref(A_RegDeref(sizeOf(ty), A_MemOffset(sizeOf(ty), A_Reg(sizeOf(ty), A_RegName.R11), A_OffsetImm(-i * sizeOfBytes(ty)))), A_Reg(sizeOf(ty), A_RegName.RetReg))
    }

    builder.toList

private def genNewPair(x1: T_Expr, x2: T_Expr, ty1: SemType, ty2: SemType)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    builder += A_Mov(A_Reg(A_OperandSize.A_32, A_RegName.R1), A_Imm(PAIR_SIZE_BYTES))
    builder += A_Call(A_ExternalLabel("malloc"))
    builder += A_Mov(A_Reg(A_OperandSize.A_64, A_RegName.R11), A_Reg(A_OperandSize.A_64, A_RegName.RetReg))
    builder ++= gen(x1)
    builder += A_MovDeref(A_RegDeref(sizeOf(ty1), A_MemOffset(A_OperandSize.A_64, A_Reg(A_OperandSize.A_64, A_RegName.R11), A_OffsetImm(ZERO_IMM))), A_Reg(sizeOf(ty1), A_RegName.RetReg))
    builder ++= gen(x2)
    builder += A_MovDeref(A_RegDeref(sizeOf(ty2), A_MemOffset(A_OperandSize.A_64, A_Reg(A_OperandSize.A_64, A_RegName.R11), A_OffsetImm(PAIR_OFFSET_SIZE))), A_Reg(sizeOf(ty2), A_RegName.RetReg))

    builder.toList


private def funcLabelGen(f: T_Name): A_InstrLabel = A_InstrLabel(s".F.${f.name}")

def sizeOf(ty: SemType): A_OperandSize = ty match
    case ? => throw Exception("Should not have semType ? in codeGen")
    case X => throw Exception("Should not have semType X in codeGen")
    case wacc.KnownType.Int => A_OperandSize.A_32
    case wacc.KnownType.Boolean => A_OperandSize.A_8
    case wacc.KnownType.Char => A_OperandSize.A_8
    case wacc.KnownType.String => ???
    case wacc.KnownType.Array(ty) => ???
    case KnownType.Pair(_, _) => ??? // should be 16 bytes - A_128??
    case KnownType.Ident => ???

def sizeOfBytes(ty: SemType): BigInt = ty match
    case ? => ???
    case X => ???
    case wacc.KnownType.Int => ???
    case wacc.KnownType.Boolean => ???
    case wacc.KnownType.Char => ???
    case wacc.KnownType.String => ???
    case wacc.KnownType.Array(ty) => ???
    case KnownType.Pair(ty1, ty2) => ???
    case KnownType.Ident => ???
