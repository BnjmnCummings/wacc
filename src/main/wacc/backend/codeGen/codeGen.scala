package wacc.codeGen

import wacc.t_ast.*
import wacc.q_ast.Name
import wacc.assemblyIR.*
import wacc.TypeInfo
import wacc.ast.PairIndex

import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.collection.immutable
import wacc.SemType
import wacc.?
import wacc.X
import wacc.KnownType
import wacc.EXIT_SUCCESS

val TRUE = 1
val FALSE = 0
val ZERO_IMM = 0
val CHR_MASK = -128
val PAIR_SIZE_BYTES = 16
val PAIR_OFFSET_SIZE = 8

def gen(t_tree: T_Prog, typeInfo: TypeInfo): A_Prog = {
    given ctx: CodeGenCtx = CodeGenCtx(typeInfo)

    val _funcs = t_tree.funcs.map(gen) ++ ctx.defaultFuncsList

    // --- generating main function ---
    // calculate frame size and add variables to stack table
    val (stackTable, frameSize) = createStackTable(t_tree.scoped, typeInfo)

    // building main function body
    val builder: ListBuffer[A_Instr] = ListBuffer()

    builder += A_Push(A_Reg(PTR_SIZE, A_RegName.BasePtr))
    builder += A_Sub(A_Reg(PTR_SIZE, A_RegName.StackPtr), A_Imm(frameSize), PTR_SIZE)
    builder += A_MovTo(A_Reg(PTR_SIZE, A_RegName.BasePtr), A_Reg(PTR_SIZE, A_RegName.StackPtr))
    builder ++= t_tree.body.flatMap(gen(_, stackTable.toMap))
    builder += A_MovTo(A_Reg(PTR_SIZE, A_RegName.RetReg), A_Imm(EXIT_SUCCESS))
    builder += A_Add(A_Reg(PTR_SIZE, A_RegName.StackPtr), A_Imm(frameSize), PTR_SIZE)
    builder += A_Pop(A_Reg(PTR_SIZE, A_RegName.BasePtr))
    builder += A_Ret

    val main = A_Func(A_InstrLabel("main"), builder.toList)

    A_Prog(ctx.storedStringsList, main :: _funcs)
}

def createStackTable(scope: Set[Name], typeInfo: TypeInfo): (mutable.Map[Name, Int], Int) = {
    val stackTable: mutable.Map[Name, Int] = mutable.Map()

    var frameSize: Int = 0
    scope.foreach(v =>
        stackTable(v) = frameSize
        frameSize += intSizeOf(typeInfo.varTys(v))
    )

    (stackTable, frameSize)
}

private def gen(t: T_Stmt, stackTable: immutable.Map[Name, Int])(using ctx: CodeGenCtx): List[A_Instr] = t match
    case T_Decl(v, r, ty) => genDecl(v, r, ty, stackTable)
    case T_Asgn(l, r, ty) => genAsgn(l, r, ty, stackTable)
    case T_Read(l, ty) => genRead(l, ty, stackTable)
    case T_Free(x, ty) => genFree(x, ty, stackTable)
    case T_Return(x, ty) => genReturn(x, ty, stackTable)
    case T_Exit(x) => genExit(x, stackTable)
    case T_Print(x, ty) => genPrint(x, ty, stackTable)
    case T_Println(x, ty) => genPrintln(x, ty, stackTable)
    case T_If(cond, body, scopedBody, el, scopedEl) => genIf(cond, body, scopedBody, el, scopedEl, stackTable)
    case T_While(cond, body, scoped) => genWhile(cond, body, scoped, stackTable)
    case T_CodeBlock(body, scoped) => genCodeBlock(body, scoped, stackTable)
    case T_Skip() => genSkip()

private def gen(t: T_Expr, stackTable: immutable.Map[Name, Int])(using ctx: CodeGenCtx): List[A_Instr] = t match
    case T_Mul(x, y) => genMul(x, y, stackTable)
    case T_Div(x, y) => genDivMod(x, y, A_RegName.RetReg, stackTable)
    case T_Mod(x, y) => genDivMod(x, y, A_RegName.R3, stackTable)
    case T_Add(x, y) => genAddSub(x, y, A_Add.apply, stackTable)
    case T_Sub(x, y) => genAddSub(x, y, A_Sub.apply, stackTable)
    case T_GreaterThan(x, y, ty) => genComparison(x, y, ty, A_Cond.Gt, stackTable)
    case T_GreaterThanEq(x, y, ty) => genComparison(x, y, ty, A_Cond.GEq, stackTable)
    case T_LessThan(x, y, ty) => genComparison(x, y, ty, A_Cond.Lt, stackTable)
    case T_LessThanEq(x, y, ty) => genComparison(x, y, ty, A_Cond.LEq, stackTable)
    case T_Eq(x, y, ty) => genComparison(x, y, ty, A_Cond.Eq, stackTable)
    case T_NotEq(x, y, ty) => genComparison(x, y, ty, A_Cond.NEq, stackTable)
    case T_And(x, y) => genBitwiseOp(x, y, A_And.apply, stackTable)
    case T_Or(x, y) => genBitwiseOp(x, y, A_Or.apply, stackTable)
    case T_Not(x) => genNot(x, stackTable)
    case T_Neg(x) => genNeg(x, stackTable)
    case T_Len(x) => genLen(x, stackTable)
    case T_Ord(x) => genOrd(x, stackTable)
    case T_Chr(x) => genChr(x, stackTable)
    case T_IntLiteral(v) => genIntLiteral(v)
    case T_BoolLiteral(v) => genBoolLiteral(v)
    case T_CharLiteral(v) => genCharLiteral(v)
    case T_StringLiteral(v) => genStringLiteral(v)
    case T_Ident(v) => genIdent(v, stackTable)
    case T_ArrayElem(v, indices) => genArrayElem(v, indices, stackTable)
    case T_PairNullLiteral => genPairNullLiteral()
    case T_PairElem(index, v) => genPairElem(index, v, stackTable)

private def gen(t: T_LValue, stackTable: immutable.Map[Name, Int])(using ctx: CodeGenCtx): List[A_Instr] = t match
    case T_Ident(v) => genIdent(v, stackTable)
    case T_ArrayElem(v, indices) => genArrayElem(v, indices, stackTable)
    case T_PairElem(index, v) => genPairElem(index, v, stackTable)

private def gen(t: T_RValue, stackTable: immutable.Map[Name, Int])(using ctx: CodeGenCtx): List[A_Instr] = t match
    case T_FuncCall(v, args) => genFuncCall(v, args, stackTable)
    case T_ArrayLiteral(xs, ty, length) => genArrayLiteral(xs, ty, length, stackTable)
    case T_NewPair(x1, x2, ty1, ty2) => genNewPair(x1, x2, ty1, ty2, stackTable)
    case _ => gen(t.asInstanceOf[T_Expr], stackTable)

private def gen(t: T_Func)(using ctx: CodeGenCtx): A_Func = {
    // --- generating function ---
    // calculate frame size and add variables to stack table
    val (stackTable, frameSize) = createStackTable(t.scoped, ctx.typeInfo)

    // building function body
    val builder: ListBuffer[A_Instr] = ListBuffer()

    builder += A_Push(A_Reg(PTR_SIZE, A_RegName.BasePtr))
    builder += A_MovTo(A_Reg(PTR_SIZE, A_RegName.BasePtr), A_Reg(PTR_SIZE, A_RegName.StackPtr))
    builder ++= t.body.flatMap(gen(_, stackTable.toMap))
    builder += A_Pop(A_Reg(PTR_SIZE, A_RegName.BasePtr))
    builder += A_Ret

    A_Func(funcLabelGen(t.v), builder.toList)
}

private def genDecl(v: Name, r: T_RValue, ty: SemType, stackTable: immutable.Map[Name, Int])(using ctx: CodeGenCtx): List[A_Instr] = genAsgn(T_Ident(v), r, ty, stackTable)

private def genAsgn(l: T_LValue, r: T_RValue, ty: SemType, stackTable: immutable.Map[Name, Int])(using ctx: CodeGenCtx): List[A_Instr] = {
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(r, stackTable)

    l match
        case T_Ident(v) =>
            // need to be able to move it into the offset... help @Zakk
            builder += A_MovFrom(A_MemOffset(sizeOf(ty), A_Reg(PTR_SIZE, A_RegName.BasePtr), A_OffsetImm(- stackTable(v))), A_Reg(sizeOf(ty), A_RegName.RetReg))
        case T_ArrayElem(v, indices) => ???
        case T_PairElem(index, v) => ???
    
    builder.toList
}

private def genRead(l: T_LValue, ty: SemType, stackTable: immutable.Map[Name, Int])(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]
    
    builder += A_MovTo(A_Reg(sizeOf(ty), A_RegName.R1), A_Reg(sizeOf(ty), A_RegName.R10))
    builder += A_Call(A_InstrLabel(s"_read${{typeToLetter(ty)}}"))
    // Result left in eax

    builder.toList

private def genFree(x: T_Expr, ty: SemType, stackTable: immutable.Map[Name, Int])(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x, stackTable)

    builder += A_MovTo(A_Reg(PTR_SIZE, A_RegName.R1), A_Reg(PTR_SIZE, A_RegName.R11))
    
    ty match
        case KnownType.Array(_) =>
            builder += A_Sub(A_Reg(PTR_SIZE, A_RegName.R1), A_Imm(opSizeToInt(INT_SIZE)), INT_SIZE)
            builder += A_Call(A_InstrLabel("_free"))
        case KnownType.Pair(_, _) =>
            builder += A_Call(A_InstrLabel("_freepair"))
        case _ => throw Exception("Invalid type with free. Should be caught in type checker!")
    
    builder.toList

private def genReturn(x: T_Expr, ty: SemType, stackTable: immutable.Map[Name, Int])(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x, stackTable)

    builder += A_MovTo(A_Reg(PTR_SIZE, A_RegName.StackPtr), A_Reg(PTR_SIZE, A_RegName.BasePtr))
    builder += A_Pop(A_Reg(PTR_SIZE, A_RegName.R10))
    builder += A_Pop(A_Reg(PTR_SIZE, A_RegName.BasePtr))
    builder += A_Ret

    builder.toList

private def genExit(x: T_Expr, stackTable: immutable.Map[Name, Int])(using ctx: CodeGenCtx): List[A_Instr] = ???

private def genPrint(x: T_Expr, ty: SemType, stackTable: immutable.Map[Name, Int])(using ctx: CodeGenCtx): List[A_Instr] = ???

private def genPrintln(x: T_Expr, ty: SemType, stackTable: immutable.Map[Name, Int])(using ctx: CodeGenCtx): List[A_Instr] = ???

private def genIfHelper(cond: T_Expr, body: List[T_Stmt], el: List[T_Stmt], stackTable: immutable.Map[Name, Int])(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    val bodyLabel = ctx.genNextInstrLabel() // .L0
    val restLabel = ctx.genNextInstrLabel() // .L1

    builder ++= gen(cond, stackTable)
    builder += A_Cmp(A_Reg(BOOL_SIZE, A_RegName.RetReg), A_Imm(TRUE), BOOL_SIZE)
    builder += A_Jmp(bodyLabel, A_Cond.Eq)

    el.foreach(builder ++= gen(_, stackTable))

    builder += A_Jmp(restLabel, A_Cond.Uncond) 

    builder += A_LabelStart(bodyLabel)
    body.foreach(builder ++= gen(_, stackTable))
    builder += A_LabelStart(restLabel)

    builder.toList

private def genIf(cond: T_Expr, body: List[T_Stmt], scopedBody: Set[Name], el: List[T_Stmt], scopedEl: Set[Name], stackTable: immutable.Map[Name, Int])(using ctx: CodeGenCtx): List[A_Instr] = 
    val builder = new ListBuffer[A_Instr]

    cond match
        case T_GreaterThan(x, y, ty) => builder ++= genIfHelper(cond, body, el, stackTable)
        case T_GreaterThanEq(x, y, ty) => builder ++= genIfHelper(cond, body, el, stackTable)
        case T_LessThan(x, y, ty) => builder ++= genIfHelper(cond, body, el, stackTable)
        case T_LessThanEq(x, y, ty) => builder ++= genIfHelper(cond, body, el, stackTable)
        case T_Eq(x, y, ty) => builder ++= genIfHelper(cond, body, el, stackTable)
        case T_NotEq(x, y, ty) => builder ++= genIfHelper(cond, body, el, stackTable)
        case T_And(x, y) => builder ++= genIfHelper(cond, body, el, stackTable)
        case T_Or(x, y) => builder ++= genIfHelper(cond, body, el, stackTable)
        case T_Not(x) => builder ++= genIfHelper(cond, body, el, stackTable)
        case T_BoolLiteral(v) => builder ++= genIfHelper(cond, body, el, stackTable)
        case T_Ident(v) => ???
        case _ => throw Exception(s"Should not reach here. Got $cond")

    builder.toList

private def genWhile(cond: T_Expr, body: List[T_Stmt], scoped: Set[Name], stackTable: immutable.Map[Name, Int])(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    val condLabel = ctx.genNextInstrLabel() // .L0
    val bodyLabel = ctx.genNextInstrLabel() // .L1

    builder += A_Jmp(condLabel, A_Cond.Uncond)
    
    builder += A_LabelStart(bodyLabel)
    body.foreach(builder ++= gen(_, stackTable)) 

    builder += A_LabelStart(condLabel)
    builder ++= gen(cond, stackTable)
    builder += A_Cmp(A_Reg(BOOL_SIZE, A_RegName.RetReg), A_Imm(TRUE), BOOL_SIZE)
    builder += A_Jmp(bodyLabel, A_Cond.Eq)

    builder.toList

private def genCodeBlock(body: List[T_Stmt], scoped: Set[Name], stackTable: immutable.Map[Name, Int])(using ctx: CodeGenCtx): List[A_Instr] = ???

private def genSkip(): List[A_Instr] = List()

private def genDivMod(x: T_Expr, y: T_Expr, divResultReg: A_RegName, stackTable: immutable.Map[Name, Int])(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    // Note: With IDiv, we need the numerator to be stored in eax and then we can divide by a given register
    // Note: IDiv stores remainder in edx(32bit) - R3

    builder ++= gen(x, stackTable)
    builder += A_Push(A_Reg(INT_SIZE, A_RegName.RetReg))
    builder ++= gen(y, stackTable)
    builder += A_MovTo(A_Reg(INT_SIZE, A_RegName.RetReg), A_Reg(INT_SIZE, A_RegName.R1))

    // Compare denominator with 0
    builder += A_Cmp(A_Reg(INT_SIZE, A_RegName.R1), A_Imm(0), INT_SIZE)
    builder += A_Jmp(???, A_Cond.Eq)
    // Above is a comparison of y (denominator) with 0
    // TODO @Aidan: Add a divide by 0 flag + label

    builder += A_Pop(A_Reg(INT_SIZE, A_RegName.RetReg))
    builder += A_IDiv(A_Reg(INT_SIZE, A_RegName.R1), INT_SIZE)
    // TODO @Aidan: Overflow can occur here - add flag system etc.
    // ^ This is the case of dividing -2^31 by -1 and getting 2^31 > 1 + 2^31 --> overflow

    builder += A_MovTo(A_Reg(INT_SIZE, A_RegName.RetReg), (A_Reg(INT_SIZE, divResultReg)))

    builder.toList

private def genAddSub(x: T_Expr, y: T_Expr, instrApply: ((A_Reg, A_Operand, A_OperandSize) => A_Instr), stackTable: immutable.Map[Name, Int])(using ctx: CodeGenCtx) =
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x, stackTable)
    builder += A_Push(A_Reg(INT_SIZE, A_RegName.RetReg))
    builder ++= gen(y, stackTable)
    builder += A_Pop(A_Reg(INT_SIZE, A_RegName.R1))
    builder += instrApply(A_Reg(INT_SIZE, A_RegName.RetReg), A_Reg(INT_SIZE, A_RegName.R1), INT_SIZE)
    // TODO @Aidan: Overflow can occur here - add flag system etc.

    builder.toList

private def genMul(x: T_Expr, y: T_Expr, stackTable: immutable.Map[Name, Int])(using ctx: CodeGenCtx): List[A_Instr] = 
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x, stackTable)
    builder += A_Push(A_Reg(INT_SIZE, A_RegName.RetReg))
    builder ++= gen(y, stackTable)
    builder += A_Pop(A_Reg(INT_SIZE, A_RegName.R1))
    builder += A_IMul(A_Reg(INT_SIZE, A_RegName.RetReg), A_Reg(INT_SIZE, A_RegName.RetReg), A_Reg(INT_SIZE, A_RegName.R1), INT_SIZE)
    // TODO @Aidan: Overflow can occur here - add flag system etc.

    builder.toList

private def genComparison(x: T_Expr, y: T_Expr, ty: SemType, cond: A_Cond, stackTable: immutable.Map[Name, Int])(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x, stackTable)
    
    val sizeTy = sizeOf(ty)

    builder += A_Push(A_Reg(sizeTy, A_RegName.RetReg))
    builder ++= gen(y, stackTable)
    builder += A_Pop(A_Reg(sizeTy, A_RegName.R1))
    builder += A_Cmp(A_Reg(sizeTy, A_RegName.R1), A_Reg(sizeTy, A_RegName.RetReg), sizeTy)
    builder += A_Set(A_Reg(BOOL_SIZE, A_RegName.RetReg), cond)

    builder.toList

private def genBitwiseOp(x: T_Expr, y: T_Expr, instrApply: ((A_Reg, A_Operand, A_OperandSize) => A_Instr), stackTable: immutable.Map[Name, Int])(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x, stackTable)
    builder += A_Push(A_Reg(BOOL_SIZE, A_RegName.RetReg))
    builder ++= gen(y, stackTable)
    builder += A_Pop(A_Reg(BOOL_SIZE, A_RegName.R1))
    builder += instrApply(A_Reg(BOOL_SIZE, A_RegName.RetReg), A_Reg(BOOL_SIZE, A_RegName.R1), BOOL_SIZE)

    builder.toList

private def genNot(x: T_Expr, stackTable: immutable.Map[Name, Int])(using ctx: CodeGenCtx): List[A_Instr] = 
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x, stackTable)
    builder += A_Xor(A_Reg(BOOL_SIZE, A_RegName.RetReg), A_Imm(TRUE), BOOL_SIZE)

    builder.toList

private def genNeg(x: T_Expr, stackTable: immutable.Map[Name, Int])(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x, stackTable)

    // CONSIDER: DO WE NEED TO SAVE R1 BEFORE THIS?
    builder += A_MovTo(A_Reg(INT_SIZE, A_RegName.R1), A_Imm(ZERO_IMM))
    builder += A_Sub(A_Reg(INT_SIZE, A_RegName.R1), A_Reg(INT_SIZE, A_RegName.RetReg), INT_SIZE)
    // check overflow -2^32 case! TODO @Aidan

    builder.toList

private def genLen(x: T_Expr, stackTable: immutable.Map[Name, Int])(using ctx: CodeGenCtx): List[A_Instr] = ???

private def genOrd(x: T_Expr, stackTable: immutable.Map[Name, Int])(using ctx: CodeGenCtx): List[A_Instr] = 
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x, stackTable)
    builder += A_Movzx(A_Reg(INT_SIZE, A_RegName.R1), A_Reg(CHAR_SIZE, A_RegName.RetReg))

    builder.toList

private def genChr(x: T_Expr, stackTable: immutable.Map[Name, Int])(using ctx: CodeGenCtx): List[A_Instr] = 
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x, stackTable)

    builder += A_MovTo(A_Reg(INT_SIZE, A_RegName.R1), A_Reg(INT_SIZE, A_RegName.RetReg))
    builder += A_And(A_Reg(INT_SIZE, A_RegName.R1), A_Imm(CHR_MASK), INT_SIZE)
    builder += A_Cmp(A_Reg(INT_SIZE, A_RegName.R1), A_Imm(ZERO_IMM), INT_SIZE)
    builder += A_Jmp(???, A_Cond.NEq)
    // TODO: @Aidan Create bad character label - this is when you chr(x) |x| > 127 (0b1111111)

    builder.toList

private def genIntLiteral(v: BigInt)(using ctx: CodeGenCtx): List[A_Instr] = List(A_MovTo(A_Reg(INT_SIZE, A_RegName.RetReg), A_Imm(v)))

private def genBoolLiteral(v: Boolean)(using ctx: CodeGenCtx): List[A_Instr] = List(A_MovTo(A_Reg(BOOL_SIZE, A_RegName.RetReg), A_Imm(if v then TRUE else FALSE)))

private def genCharLiteral(v: Char)(using ctx: CodeGenCtx): List[A_Instr] = List(A_MovTo(A_Reg(CHAR_SIZE, A_RegName.RetReg), A_Imm(v.toInt)))

private def genStringLiteral(v: String)(using ctx: CodeGenCtx): List[A_Instr] = {
    val lbl = ctx.genStoredStr(v)

    val offset = A_MemOffset(PTR_SIZE, A_Reg(PTR_SIZE, A_RegName.InstrPtr), A_OffsetLbl(lbl))

    List(A_Lea(A_Reg(PTR_SIZE, A_RegName.RetReg), offset))
}

private def genIdent(v: Name, stackTable: immutable.Map[Name, Int])(using ctx: CodeGenCtx): List[A_Instr] = 
    List(A_MovTo(A_Reg(sizeOf(ctx.typeInfo.varTys(v)), A_RegName.RetReg), A_MemOffset(sizeOf(ctx.typeInfo.varTys(v)), A_Reg(PTR_SIZE, A_RegName.StackPtr), A_OffsetImm(stackTable(v)))))

private def genArrayElem(v: Name, indices: List[T_Expr], stackTable: immutable.Map[Name, Int])(using ctx: CodeGenCtx): List[A_Instr] = ???

private def genPairNullLiteral()(using ctx: CodeGenCtx): List[A_Instr] = ???

private def genPairElem(index: PairIndex, v: T_LValue, stackTable: immutable.Map[Name, Int])(using ctx: CodeGenCtx): List[A_Instr] = ???

private def genFuncCall(v: Name, args: List[T_Expr], stackTable: immutable.Map[Name, Int])(using ctx: CodeGenCtx): List[A_Instr] = {

    val builder = new ListBuffer[A_Instr]

    val argNames: List[Name] = ctx.typeInfo.funcTys(v)._2

    val (newStackTable, frameSize) = createStackTable(argNames.toSet, ctx.typeInfo)

    val offsetOldStackTable = stackTable.map((k, v) => (k, v + frameSize))

    builder += A_Sub(A_Reg(PTR_SIZE, A_RegName.StackPtr), A_Imm(frameSize), PTR_SIZE)
    argNames.zip(args).foreach((arg, expr) => {
        builder ++= gen(expr, offsetOldStackTable)
        builder += A_MovFrom(A_MemOffset(sizeOf(ctx.typeInfo.varTys(arg)), A_Reg(PTR_SIZE, A_RegName.StackPtr), A_OffsetImm(-newStackTable(arg))), A_Reg(sizeOf(ctx.typeInfo.varTys(arg)), A_RegName.RetReg))
    })

    builder += A_Call(funcLabelGen(v))

    builder += A_Add(A_Reg(PTR_SIZE, A_RegName.StackPtr), A_Imm(frameSize), PTR_SIZE)

    builder.toList
}

private def genArrayLiteral(xs: List[T_Expr], ty: SemType, length: BigInt, stackTable: immutable.Map[Name, Int])(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]
    val sizeBytes = opSizeToInt(INT_SIZE) + (intSizeOf(ty) * length)

    builder += A_MovTo(A_Reg(INT_SIZE, A_RegName.R1), A_Imm(sizeBytes))
    builder += A_Call(A_ExternalLabel("malloc"))
    builder += A_MovTo(A_Reg(PTR_SIZE, A_RegName.R11), A_Reg(PTR_SIZE, A_RegName.RetReg))
    builder += A_Add(A_Reg(PTR_SIZE, A_RegName.R11), A_Imm(opSizeToInt(INT_SIZE)), INT_SIZE)
    builder += A_MovDeref(A_RegDeref(sizeOf(ty), A_MemOffset(sizeOf(ty), A_Reg(sizeOf(ty), A_RegName.R11), A_OffsetImm(-opSizeToInt(INT_SIZE)))), A_Imm(length))

    for (i <- 0 to length.asInstanceOf[Int]) { // TODO: as instance of used! haha (ranges won't take BigInt - refactor or leave?)
        builder ++= gen(xs(i), stackTable)
        builder += A_MovDeref(A_RegDeref(sizeOf(ty), A_MemOffset(sizeOf(ty), A_Reg(sizeOf(ty), A_RegName.R11), A_OffsetImm(-i * intSizeOf(ty)))), A_Reg(sizeOf(ty), A_RegName.RetReg))
    }

    builder.toList

private def genNewPair(x1: T_Expr, x2: T_Expr, ty1: SemType, ty2: SemType, stackTable: immutable.Map[Name, Int])(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    builder += A_MovTo(A_Reg(INT_SIZE, A_RegName.R1), A_Imm(opSizeToInt(PTR_SIZE) * 2))
    builder += A_Call(A_ExternalLabel("malloc"))
    builder += A_MovTo(A_Reg(PTR_SIZE, A_RegName.R11), A_Reg(PTR_SIZE, A_RegName.RetReg))
    builder ++= gen(x1, stackTable)
    builder += A_MovDeref(A_RegDeref(sizeOf(ty1), A_MemOffset(PTR_SIZE, A_Reg(PTR_SIZE, A_RegName.R11), A_OffsetImm(ZERO_IMM))), A_Reg(sizeOf(ty1), A_RegName.RetReg))
    builder ++= gen(x2, stackTable)
    builder += A_MovDeref(A_RegDeref(sizeOf(ty2), A_MemOffset(PTR_SIZE, A_Reg(PTR_SIZE, A_RegName.R11), A_OffsetImm(PAIR_OFFSET_SIZE))), A_Reg(sizeOf(ty2), A_RegName.RetReg))

    builder.toList


private def funcLabelGen(f: Name): A_InstrLabel = A_InstrLabel(s".F.${f.name}")

def sizeOf(ty: SemType): A_OperandSize = ty match
    case ? => throw Exception("Should not have semType ? in codeGen")
    case X => throw Exception("Should not have semType X in codeGen")
    case wacc.KnownType.Int => INT_SIZE
    case wacc.KnownType.Boolean => BOOL_SIZE
    case wacc.KnownType.Char => CHAR_SIZE
    case wacc.KnownType.String => PTR_SIZE
    case wacc.KnownType.Array(ty) => PTR_SIZE
    case KnownType.Pair(_, _) => ???
    case KnownType.Ident => ???

def typeToLetter(ty: SemType): String = ty match
    case ? => ???
    case X => ???
    case wacc.KnownType.Int => "i"
    case wacc.KnownType.Boolean => ???
    case wacc.KnownType.Char => ???
    case wacc.KnownType.String => ???
    case wacc.KnownType.Array(ty) => ???
    case KnownType.Pair(ty1, ty2) => ???
    case KnownType.Ident => ???

def opSizeToInt(opSize: A_OperandSize): Int = opSize match
    case A_OperandSize.A_8 => 1
    case A_OperandSize.A_16 => 2
    case A_OperandSize.A_32 => 4
    case A_OperandSize.A_64 => 8

def intSizeOf(ty: SemType): Int = opSizeToInt(sizeOf(ty)) 