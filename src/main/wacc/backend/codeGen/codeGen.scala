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

val MAIN_FUNC_NAME = "main"

def gen(t_tree: T_Prog, typeInfo: TypeInfo): A_Prog = {
    given ctx: CodeGenCtx = CodeGenCtx(typeInfo, getTables(t_tree, typeInfo))

    val _funcs = t_tree.funcs.map(gen)// ++ ctx.defaultFuncsList

    // building main function body
    val builder: ListBuffer[A_Instr] = ListBuffer()

    builder += A_Push(A_Reg(A_RegName.BasePtr))
    builder += A_MovTo(A_Reg(A_RegName.BasePtr), A_Reg(A_RegName.StackPtr), PTR_SIZE)
    builder += A_Sub(A_Reg(A_RegName.StackPtr), A_Imm(ctx.stackTables.mainTable.size), PTR_SIZE)
    builder ++= t_tree.body.flatMap(gen(_, ctx.stackTables.mainTable))
    builder += A_MovTo(A_Reg(A_RegName.RetReg), A_Imm(EXIT_SUCCESS), PTR_SIZE)
    builder += A_Add(A_Reg(A_RegName.StackPtr), A_Imm(ctx.stackTables.mainTable.size), PTR_SIZE)
    builder += A_Pop(A_Reg(A_RegName.BasePtr))
    builder += A_Ret

    val main = A_Func(A_InstrLabel(MAIN_FUNC_NAME), builder.toList)

    val _funcsWithDefaults = _funcs ++ ctx.defaultFuncsList

    A_Prog(ctx.storedStringsList, main :: _funcsWithDefaults)
}

private def gen(t: T_Stmt, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = t match
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

private def gen(t: T_Expr, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = t match
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
    case T_And(x, y) => genBitwiseOp(x, y, stackTable, A_Cond.NEq)
    case T_Or(x, y) => genBitwiseOp(x, y, stackTable, A_Cond.Eq)
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

private def gen(t: T_LValue, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = t match
    case T_Ident(v) => genIdent(v, stackTable)
    case T_ArrayElem(v, indices) => genArrayElem(v, indices, stackTable)
    case T_PairElem(index, v) => genPairElem(index, v, stackTable)

private def gen(t: T_RValue, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = t match
    case T_FuncCall(v, args) => genFuncCall(v, args, stackTable)
    case T_ArrayLiteral(xs, ty, length) => genArrayLiteral(xs, ty, length, stackTable)
    case T_NewPair(x1, x2, ty1, ty2) => genNewPair(x1, x2, ty1, ty2, stackTable)
    case _ => gen(t.asInstanceOf[T_Expr], stackTable)

private def gen(t: T_Func)(using ctx: CodeGenCtx): A_Func = {

    val stackTable: StackTables = ctx.stackTables.funcTables(t.v)

    // building function body
    val builder: ListBuffer[A_Instr] = ListBuffer()

    builder += A_Push(A_Reg(A_RegName.BasePtr))
    builder += A_MovTo(A_Reg(A_RegName.BasePtr), A_Reg(A_RegName.StackPtr), PTR_SIZE)
    builder += A_Sub(A_Reg(A_RegName.StackPtr), A_Imm(stackTable.size), PTR_SIZE)
    builder ++= t.body.flatMap(gen(_, stackTable))
    builder += A_MovTo(A_Reg(A_RegName.RetReg), A_Imm(EXIT_SUCCESS), PTR_SIZE)
    builder += A_Add(A_Reg(A_RegName.StackPtr), A_Imm(stackTable.size), PTR_SIZE)
    builder += A_Pop(A_Reg(A_RegName.BasePtr))
    builder += A_Ret

    A_Func(funcLabelGen(t.v), builder.toList)
}

private def genDecl(v: Name, r: T_RValue, ty: SemType, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = genAsgn(T_Ident(v), r, ty, stackTable)

private def genAsgn(l: T_LValue, r: T_RValue, ty: SemType, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = {
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(r, stackTable)

    l match
        case T_Ident(v) =>
            builder ++= stackTable.set(v)
        case T_ArrayElem(v, indices) => {
            val ty = unwrapArr(ctx.typeInfo.varTys(v), indices.length)

            ctx.addDefaultFunc(ERR_OUT_OF_BOUNDS_LABEL)

            builder += A_Push(A_Reg(A_RegName.RetReg))

            builder ++= genIdent(v, stackTable)

            for i <- 0 to indices.length - 2 do
                builder += A_Push(A_Reg(A_RegName.RetReg))
                builder += A_MovTo(A_Reg(A_RegName.RetReg), A_Imm(0), PTR_SIZE)
                builder ++= gen(indices(i), stackTable)
                // check in range
                builder += A_Cmp(A_Reg(A_RegName.RetReg), A_Imm(0), INT_SIZE)
                builder += A_Jmp(A_InstrLabel(ERR_OUT_OF_BOUNDS_LABEL), A_Cond.Lt)
                builder += A_Pop(A_Reg(A_RegName.R1))
                builder += A_MovTo(A_Reg(A_RegName.R2), A_RegDeref(A_MemOffset(A_Reg(A_RegName.R1), A_OffsetImm(-opSizeToInt(INT_SIZE)))), INT_SIZE)
                builder += A_Push(A_Reg(A_RegName.R1))
                builder += A_Cmp(A_Reg(A_RegName.RetReg), A_Reg(A_RegName.R2), INT_SIZE)
                builder += A_Jmp(A_InstrLabel(ERR_OUT_OF_BOUNDS_LABEL), A_Cond.GEq)

                builder += A_IMul(A_Reg(A_RegName.RetReg), A_Imm(opSizeToInt(PTR_SIZE)), PTR_SIZE)
                builder += A_Pop(A_Reg(A_RegName.R1))
                builder += A_Add(A_Reg(A_RegName.RetReg), A_Reg(A_RegName.R1), PTR_SIZE)
                builder += A_MovFromDeref(A_Reg(A_RegName.RetReg), A_RegDeref(A_MemOffset(A_Reg(A_RegName.RetReg), A_OffsetImm(0))), PTR_SIZE)
            
            builder += A_Push(A_Reg(A_RegName.RetReg))
            builder += A_MovTo(A_Reg(A_RegName.RetReg), A_Imm(0), PTR_SIZE)
            builder ++= gen(indices(indices.length - 1), stackTable)
            // check in range
            builder += A_Cmp(A_Reg(A_RegName.RetReg), A_Imm(0), INT_SIZE)
            builder += A_Jmp(A_InstrLabel(ERR_OUT_OF_BOUNDS_LABEL), A_Cond.Lt)
            builder += A_Pop(A_Reg(A_RegName.R1))
            builder += A_MovTo(A_Reg(A_RegName.R2), A_RegDeref(A_MemOffset(A_Reg(A_RegName.R1), A_OffsetImm(-opSizeToInt(INT_SIZE)))), INT_SIZE)
            builder += A_Push(A_Reg(A_RegName.R1))
            builder += A_Cmp(A_Reg(A_RegName.RetReg), A_Reg(A_RegName.R2), INT_SIZE)
            builder += A_Jmp(A_InstrLabel(ERR_OUT_OF_BOUNDS_LABEL), A_Cond.GEq)

            builder += A_IMul(A_Reg(A_RegName.RetReg), A_Imm(opSizeToInt(sizeOf(ty))), PTR_SIZE)
            builder += A_Pop(A_Reg(A_RegName.R1))
            builder += A_Add(A_Reg(A_RegName.R1), A_Reg(A_RegName.RetReg), PTR_SIZE)

            builder += A_Pop(A_Reg(A_RegName.RetReg))
            builder += A_MovFrom(A_MemOffset(A_Reg(A_RegName.R1), A_OffsetImm(0)), A_Reg(A_RegName.RetReg), sizeOf(ty))
        }
        case T_PairElem(index, v) =>
            builder += A_Push(A_Reg(A_RegName.RetReg))
            builder ++= getPairElemPtr(index, v, stackTable)
            builder += A_Pop(A_Reg(A_RegName.R1))

            // val sizeTy = ???

            builder += A_MovDeref(A_RegDeref(A_MemOffset(A_Reg(A_RegName.RetReg), A_OffsetImm(ZERO_IMM))), A_Reg(A_RegName.R1), PTR_SIZE)
    
    builder.toList
}

private def genRead(l: T_LValue, ty: SemType, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    // load current value of lvalue into register
    builder ++= gen(l, stackTable)
    builder += A_MovTo(A_Reg(A_RegName.R1), A_Reg(A_RegName.RetReg), sizeOf(ty))

    if ty == KnownType.Int then
        ctx.addDefaultFunc(READI_LABEL)
        builder += A_Call(A_InstrLabel(READI_LABEL))
    else if ty == KnownType.Char then
        ctx.addDefaultFunc(READC_LABEL)
        builder += A_Call(A_InstrLabel(READC_LABEL))
    else
        // naughty
        throw new Exception("Invalid type for read (should be caught in type checker)")

    // move eax value back into lvalue
    // note: we can still assume value is in ret reg
    l match
        case T_Ident(v) =>
            builder ++= stackTable.set(v)
        case T_ArrayElem(v, indices) => 
            builder += A_Push(A_Reg(A_RegName.RetReg))
            builder ++= getPointerToArrayElem(v, indices, stackTable)
            builder += A_Pop(A_Reg(A_RegName.R1))
            
            builder += A_MovDeref(
                A_RegDeref(A_MemOffset(A_Reg(A_RegName.RetReg), A_OffsetImm(ZERO_IMM))), 
                A_Reg(A_RegName.R1),
                sizeOf(ty)
            )
        // Read fst fst p is not allowed - we can only do read fst p
        case T_PairElem(index, T_Ident(v)) => 
            val offset = index match
                case PairIndex.First => ZERO_IMM
                case PairIndex.Second => opSizeToInt(PTR_SIZE)
            
            builder ++= stackTable.set(v)
        
        case T_PairElem(index, T_ArrayElem(v, indices)) => ???
            // see above, use genArrayElem

        case T_PairElem(_, _) => throw new Exception("Can't read from nested pairs. Should be caught in type checker")

    builder.toList

private def genFree(x: T_Expr, ty: SemType, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x, stackTable)

    builder += A_MovTo(A_Reg(A_RegName.R1), A_Reg(A_RegName.R11), PTR_SIZE)
    
    ty match
        case KnownType.Array(_) =>
            builder += A_Sub(A_Reg(A_RegName.R1), A_Imm(opSizeToInt(INT_SIZE)), PTR_SIZE)

            ctx.addDefaultFunc(FREE_LABEL)

            builder += A_Call(A_InstrLabel(FREE_LABEL))
        case KnownType.Pair(_, _) =>
            builder += A_Cmp(A_Reg(A_RegName.R1), A_Imm(ZERO_IMM), PTR_SIZE)
            builder += A_Jmp(A_InstrLabel(ERR_NULL_PAIR_LABEL), A_Cond.Eq)

            ctx.addDefaultFunc(FREE_PAIR_LABEL)
            ctx.addDefaultFunc(ERR_NULL_PAIR_LABEL)

            builder += A_Call(A_InstrLabel(FREE_PAIR_LABEL))
        case _ => throw Exception("Invalid type with free. Should be caught in type checker!")
    
    builder.toList

private def genReturn(x: T_Expr, ty: SemType, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder: ListBuffer[A_Instr] = ListBuffer()
    builder ++= gen(x, stackTable)
    builder += A_Add(A_Reg(A_RegName.StackPtr), A_Imm(stackTable.size), PTR_SIZE)
    builder += A_Pop(A_Reg(A_RegName.BasePtr))
    builder += A_Ret
    builder.toList

private def genExit(x: T_Expr, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = 
    val builder = new ListBuffer[A_Instr]

    ctx.addDefaultFunc(EXIT_LABEL)

    builder ++= gen(x, stackTable)
    // x will be an integer - we can only perform exit on integers
    builder += A_MovTo(A_Reg(A_RegName.R1), A_Reg(A_RegName.RetReg), INT_SIZE)
    // We need to move the exit code into edi (32-bit R1) for the exit code to be successfully passed to plt@exit
    builder += A_Call(A_InstrLabel(EXIT_LABEL))

    builder.toList

private def genPrint(x: T_Expr, ty: SemType, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x, stackTable)
    // x can be any type so use sizeOf(ty)
    builder += A_MovTo(A_Reg(A_RegName.R1), A_Reg(A_RegName.RetReg), sizeOf(ty))
    // We need to move x into edi (32-bit R1) for the value to be successfully passed to plt@printf

    // add the right data and functions to the context
    ty match
        case KnownType.Int => {
            ctx.addDefaultFunc(PRINTI_LABEL)
        }
        case KnownType.Boolean => {
            ctx.addDefaultFunc(PRINTB_LABEL)
        }
        case KnownType.Char => {
            ctx.addDefaultFunc(PRINTC_LABEL)
        }
        case KnownType.String => {
            ctx.addDefaultFunc(PRINTS_LABEL)
        }
        case KnownType.Array(KnownType.Char) => {
            ctx.addDefaultFunc(PRINTS_LABEL)
        }
        // here we must have a pointer print e.g. array/pair
        case _ => {
            ctx.addDefaultFunc(PRINTP_LABEL)
        }

    // call the right function
    builder += A_Call(A_InstrLabel(s"_print${{typeToLetter(ty)}}"))

    builder.toList

private def genPrintln(x: T_Expr, ty: SemType, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    builder ++= genPrint(x, ty, stackTable)
    
    ctx.addDefaultFunc(PRINTLN_LABEL)
    builder += A_Call(A_InstrLabel(PRINTLN_LABEL))

    builder.toList

private def genIfHelper(cond: T_Expr, body: List[T_Stmt], scopedBody: Set[Name], el: List[T_Stmt], scopedEl: Set[Name], stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    val bodyLabel = ctx.genNextInstrLabel() // .L0
    val restLabel = ctx.genNextInstrLabel() // .L1

    builder ++= gen(cond, stackTable)
    builder += A_Cmp(A_Reg(A_RegName.RetReg), A_Imm(TRUE), BOOL_SIZE)
    builder += A_Jmp(bodyLabel, A_Cond.Eq)

    builder ++= genCodeBlock(el, scopedEl, stackTable) 

    builder += A_Jmp(restLabel, A_Cond.Uncond) 

    builder += A_LabelStart(bodyLabel)

    // set up new stack table for body

    builder ++= genCodeBlock(body, scopedBody, stackTable)

    builder += A_LabelStart(restLabel)

    builder.toList

private def genIf(cond: T_Expr, body: List[T_Stmt], scopedBody: Set[Name], el: List[T_Stmt], scopedEl: Set[Name], stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = 
    val builder = new ListBuffer[A_Instr]

    cond match
        case T_GreaterThan(x, y, ty) => builder ++= genIfHelper(cond, body, scopedBody, el, scopedEl, stackTable)
        case T_GreaterThanEq(x, y, ty) => builder ++= genIfHelper(cond, body, scopedBody, el, scopedEl, stackTable)
        case T_LessThan(x, y, ty) => builder ++= genIfHelper(cond, body, scopedBody, el, scopedEl, stackTable)
        case T_LessThanEq(x, y, ty) => builder ++= genIfHelper(cond, body, scopedBody, el, scopedEl, stackTable)
        case T_Eq(x, y, ty) => builder ++= genIfHelper(cond, body, scopedBody, el, scopedEl, stackTable)
        case T_NotEq(x, y, ty) => builder ++= genIfHelper(cond, body, scopedBody, el, scopedEl, stackTable)
        case T_And(x, y) => builder ++= genIfHelper(cond, body, scopedBody, el, scopedEl, stackTable)
        case T_Or(x, y) => builder ++= genIfHelper(cond, body, scopedBody, el, scopedEl, stackTable)
        case T_Not(x) => builder ++= genIfHelper(cond, body, scopedBody, el, scopedEl, stackTable)
        case T_BoolLiteral(v) => builder ++= genIfHelper(cond, body, scopedBody, el, scopedEl, stackTable)
        case T_Ident(v) => builder ++= genIdent(v, stackTable) ++ genIfHelper(cond, body, scopedBody, el, scopedEl, stackTable)
        case _ => throw Exception(s"Should not reach here. Got $cond")

    builder.toList

private def genWhile(cond: T_Expr, body: List[T_Stmt], scoped: Set[Name], stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    val condLabel = ctx.genNextInstrLabel() // .L0
    val bodyLabel = ctx.genNextInstrLabel() // .L1

    builder += A_Jmp(condLabel, A_Cond.Uncond)
    
    builder += A_LabelStart(bodyLabel)

    // setup new stack table for body
    builder ++= genCodeBlock(body, scoped, stackTable)  

    builder += A_LabelStart(condLabel)
    builder ++= gen(cond, stackTable)
    builder += A_Cmp(A_Reg(A_RegName.RetReg), A_Imm(TRUE), BOOL_SIZE)
    builder += A_Jmp(bodyLabel, A_Cond.Eq)

    builder.toList

private def genCodeBlock(body: List[T_Stmt], scoped: Set[Name], stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = body.flatMap(gen(_, stackTable))

private def genSkip(): List[A_Instr] = List()

private def genDivMod(x: T_Expr, y: T_Expr, divResultReg: A_RegName, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =

    val builder = new ListBuffer[A_Instr]

    // Note: With IDiv, we need the numerator to be stored in eax and then we can divide by a given register
    // Note: IDiv stores remainder in edx(32bit) - R3

    builder ++= gen(x, stackTable)
    builder += A_Push(A_Reg(A_RegName.RetReg))
    builder ++= gen(y, stackTable)
    builder += A_MovTo(A_Reg(A_RegName.R1), A_Reg(A_RegName.RetReg), INT_SIZE)

    // Compare denominator with 0
    builder += A_Cmp(A_Reg(A_RegName.R1), A_Imm(0), INT_SIZE)

    ctx.addDefaultFunc(ERR_DIV_ZERO_LABEL)

    builder += A_Jmp(A_InstrLabel(ERR_DIV_ZERO_LABEL), A_Cond.Eq)
    // Above is a comparison of y (denominator) with 0 - error if it succeeds

    builder += A_Pop(A_Reg(A_RegName.RetReg))
    builder += A_CDQ
    builder += A_IDiv(A_Reg(A_RegName.R1), INT_SIZE)

    ctx.addDefaultFunc(ERR_OVERFLOW_LABEL)

    builder += A_Jmp(A_InstrLabel(ERR_OVERFLOW_LABEL), A_Cond.Overflow)
    // ^ This is the case of dividing -2^31 by -1 and getting 2^31 > 1 + 2^31 --> overflow

    builder += A_MovTo(A_Reg(A_RegName.RetReg), (A_Reg(divResultReg)), INT_SIZE)

    builder.toList

private def genAddSub(x: T_Expr, y: T_Expr, instrApply: ((A_Reg, A_Operand, A_OperandSize) => A_Instr), stackTable: StackTables)(using ctx: CodeGenCtx) =

    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x, stackTable)
    builder += A_Push(A_Reg(A_RegName.RetReg))
    builder ++= gen(y, stackTable)
    builder += A_MovTo(A_Reg(A_RegName.R1), A_Reg(A_RegName.RetReg), INT_SIZE)
    builder += A_Pop(A_Reg(A_RegName.RetReg))
    builder += instrApply(A_Reg(A_RegName.RetReg), A_Reg(A_RegName.R1), INT_SIZE)

    ctx.addDefaultFunc(ERR_OVERFLOW_LABEL)

    builder += A_Jmp(A_InstrLabel(ERR_OVERFLOW_LABEL), A_Cond.Overflow)

    builder.toList

private def genMul(x: T_Expr, y: T_Expr, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = 
    
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x, stackTable)
    builder += A_Push(A_Reg(A_RegName.RetReg))
    builder ++= gen(y, stackTable)
    builder += A_Pop(A_Reg(A_RegName.R1))
    builder += A_IMul(A_Reg(A_RegName.RetReg), A_Reg(A_RegName.R1), INT_SIZE)

    ctx.addDefaultFunc(ERR_OVERFLOW_LABEL)

    builder += A_Jmp(A_InstrLabel(ERR_OVERFLOW_LABEL), A_Cond.Overflow)

    builder.toList

private def genComparison(x: T_Expr, y: T_Expr, ty: SemType, cond: A_Cond, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x, stackTable)
    
    val sizeTy = sizeOf(ty)

    builder += A_Push(A_Reg(A_RegName.RetReg))
    builder ++= gen(y, stackTable)
    builder += A_Pop(A_Reg(A_RegName.R1))
    builder += A_Cmp(A_Reg(A_RegName.R1), A_Reg(A_RegName.RetReg), sizeTy)
    builder += A_Set(A_Reg(A_RegName.RetReg), cond)

    builder.toList

private def genBitwiseOp(x: T_Expr, y: T_Expr, stackTable: StackTables, cond: A_Cond)(using ctx: CodeGenCtx): List[A_Instr] =
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

private def genNot(x: T_Expr, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = 
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x, stackTable)
    builder += A_Xor(A_Reg(A_RegName.RetReg), A_Imm(TRUE), BOOL_SIZE)

    builder.toList

private def genNeg(x: T_Expr, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =    
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x, stackTable)

    // CONSIDER: DO WE NEED TO SAVE R1 BEFORE THIS?
    builder += A_MovTo(A_Reg(A_RegName.R1), A_Imm(ZERO_IMM), INT_SIZE)
    builder += A_Sub(A_Reg(A_RegName.R1), A_Reg(A_RegName.RetReg), INT_SIZE)

    ctx.addDefaultFunc(ERR_OVERFLOW_LABEL)

    builder += A_Jmp(A_InstrLabel(ERR_OVERFLOW_LABEL), A_Cond.Overflow)
    builder += A_MovTo(A_Reg(A_RegName.RetReg), A_Reg(A_RegName.R1), INT_SIZE)
    // ^ overflow -2^32 case!

    builder.toList

private def genLen(x: T_Expr, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    // PRE: WE KNOW x IS A LIST BECAUSE OF TYPE CHECKING
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x, stackTable)
    // We now have the pointer to the first element stored in RAX (64-bit RetReg)
    // We know the size is stored 4 bytes before the first element hence we can do a reg deref of retreg -4 to find the size
    builder += A_MovFromDeref(A_Reg(A_RegName.RetReg), A_RegDeref(A_MemOffset(A_Reg(A_RegName.RetReg), A_OffsetImm(-opSizeToInt(INT_SIZE)))), INT_SIZE)

    builder.toList

private def genOrd(x: T_Expr, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = 
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x, stackTable)
    builder += A_Movzx(A_Reg(A_RegName.R1), A_Reg(A_RegName.RetReg), INT_SIZE, CHAR_SIZE)

    builder.toList

private def genChr(x: T_Expr, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = 
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x, stackTable)

    builder += A_MovTo(A_Reg(A_RegName.R1), A_Reg(A_RegName.RetReg), INT_SIZE)
    builder += A_And(A_Reg(A_RegName.R1), A_Imm(-128), INT_SIZE)

    ctx.addDefaultFunc(ERR_BAD_CHAR_LABEL)
    builder += A_Jmp(A_InstrLabel(ERR_BAD_CHAR_LABEL), A_Cond.NEq)

    builder.toList

private def genIntLiteral(v: BigInt)(using ctx: CodeGenCtx): List[A_Instr] = List(A_MovTo(A_Reg(A_RegName.RetReg), A_Imm(v), INT_SIZE))

private def genBoolLiteral(v: Boolean)(using ctx: CodeGenCtx): List[A_Instr] = List(A_MovTo(A_Reg(A_RegName.RetReg), A_Imm(if v then TRUE else FALSE), BOOL_SIZE))

private def genCharLiteral(v: Char)(using ctx: CodeGenCtx): List[A_Instr] = List(A_MovTo(A_Reg(A_RegName.RetReg), A_Imm(v.toInt), CHAR_SIZE))

private def genStringLiteral(v: String)(using ctx: CodeGenCtx): List[A_Instr] = 
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
    List(A_Lea(A_Reg(A_RegName.RetReg), A_MemOffset(A_Reg(A_RegName.InstrPtr), A_OffsetLbl(ctx.genStoredStr(str)))))

private def genIdent(v: Name, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = 
    stackTable.get(v).toList

private def unwrapArr(ty: KnownType, length: Int): SemType = (ty, length) match
    case (_, 0) => ty
    case (wacc.KnownType.Array(t), _) => unwrapArr(t.asInstanceOf[KnownType], length - 1)
    case _ => throw Exception(s"Received a type that isn't an array: $ty")

private def getPointerToArrayElem(v: Name, indices: List[T_Expr], stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder: ListBuffer[A_Instr] = ListBuffer()

    val ty = unwrapArr(ctx.typeInfo.varTys(v), indices.length)

    ctx.addDefaultFunc(ERR_OUT_OF_BOUNDS_LABEL)

    builder ++= genIdent(v, stackTable)

    for i <- 0 to indices.length - 2 do
        builder += A_Push(A_Reg(A_RegName.RetReg))
        builder += A_MovTo(A_Reg(A_RegName.RetReg), A_Imm(0), PTR_SIZE)
        builder ++= gen(indices(i), stackTable)
        // check in range
        builder += A_Cmp(A_Reg(A_RegName.RetReg), A_Imm(0), INT_SIZE)
        builder += A_Jmp(A_InstrLabel(ERR_OUT_OF_BOUNDS_LABEL), A_Cond.Lt)
        builder += A_Pop(A_Reg(A_RegName.R1))
        builder += A_MovTo(A_Reg(A_RegName.R2), A_RegDeref(A_MemOffset(A_Reg(A_RegName.R1), A_OffsetImm(-opSizeToInt(INT_SIZE)))), INT_SIZE)
        builder += A_Push(A_Reg(A_RegName.R1))
        builder += A_Cmp(A_Reg(A_RegName.RetReg), A_Reg(A_RegName.R2), INT_SIZE)
        builder += A_Jmp(A_InstrLabel(ERR_OUT_OF_BOUNDS_LABEL), A_Cond.GEq)

        builder += A_IMul(A_Reg(A_RegName.RetReg), A_Imm(opSizeToInt(PTR_SIZE)), PTR_SIZE)
        builder += A_Pop(A_Reg(A_RegName.R1))
        builder += A_Add(A_Reg(A_RegName.RetReg), A_Reg(A_RegName.R1), PTR_SIZE)
        builder += A_MovFromDeref(A_Reg(A_RegName.RetReg), A_RegDeref(A_MemOffset(A_Reg(A_RegName.RetReg), A_OffsetImm(0))), PTR_SIZE)
    
    builder += A_Push(A_Reg(A_RegName.RetReg))
    builder += A_MovTo(A_Reg(A_RegName.RetReg), A_Imm(0), PTR_SIZE)
    builder ++= gen(indices(indices.length - 1), stackTable)
    // check in range
    builder += A_Cmp(A_Reg(A_RegName.RetReg), A_Imm(0), INT_SIZE)
    builder += A_Jmp(A_InstrLabel(ERR_OUT_OF_BOUNDS_LABEL), A_Cond.Lt)
    builder += A_Pop(A_Reg(A_RegName.R1))
    builder += A_MovTo(A_Reg(A_RegName.R2), A_RegDeref(A_MemOffset(A_Reg(A_RegName.R1), A_OffsetImm(-opSizeToInt(INT_SIZE)))), INT_SIZE)
    builder += A_Push(A_Reg(A_RegName.R1))
    builder += A_Cmp(A_Reg(A_RegName.RetReg), A_Reg(A_RegName.R2), INT_SIZE)
    builder += A_Jmp(A_InstrLabel(ERR_OUT_OF_BOUNDS_LABEL), A_Cond.GEq)

    builder += A_IMul(A_Reg(A_RegName.RetReg), A_Imm(opSizeToInt(sizeOf(ty))), PTR_SIZE)
    builder += A_Pop(A_Reg(A_RegName.R1))
    builder += A_Add(A_Reg(A_RegName.RetReg), A_Reg(A_RegName.R1), PTR_SIZE)

    builder.toList

private def genArrayElem(v: Name, indices: List[T_Expr], stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder: ListBuffer[A_Instr] = ListBuffer()

    val ty = unwrapArr(ctx.typeInfo.varTys(v), indices.length)

    builder ++= getPointerToArrayElem(v, indices, stackTable)
    builder += A_MovFromDeref(A_Reg(A_RegName.RetReg), A_RegDeref(A_MemOffset(A_Reg(A_RegName.RetReg), A_OffsetImm(ZERO_IMM))), sizeOf(ty))

    builder.toList

private def genPairNullLiteral()(using ctx: CodeGenCtx): List[A_Instr] = List(A_MovTo(A_Reg(A_RegName.RetReg), A_Imm(ZERO_IMM), PTR_SIZE))

private def getArrInnerType(ty: SemType): SemType = ty match
    case KnownType.Array(t) => getArrInnerType(t)
    case t => t

private def getPairElemPtr(index: PairIndex, v: T_LValue, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    val offset = index match
        case PairIndex.First => ZERO_IMM
        case PairIndex.Second => opSizeToInt(PTR_SIZE)

    builder ++= gen(v, stackTable)

    builder += A_Cmp(A_Reg(A_RegName.RetReg), A_Imm(ZERO_IMM), PTR_SIZE)

    ctx.addDefaultFunc(ERR_NULL_PAIR_LABEL)

    builder += A_Jmp(A_InstrLabel(ERR_NULL_PAIR_LABEL), A_Cond.Eq)
    builder += A_Add(A_Reg(A_RegName.RetReg), A_Imm(offset), PTR_SIZE)

    builder.toList

private def genPairElem(index: PairIndex, v: T_LValue, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    builder ++= getPairElemPtr(index, v, stackTable)
    // Now we have the pointer to fst/snd p in RetReg
    
    v match
        case T_Ident(name) =>
            // We assume pointer to our elem is stored in RetReg
            
            // deref this value to get value stored
            val pairTy = ctx.typeInfo.varTys(name).asInstanceOf[KnownType.Pair] // TODO: as instance of!!! (crashing out)

            val ty = index match
                case PairIndex.First => pairTy.ty1
                case PairIndex.Second => pairTy.ty2

            builder += A_MovFromDeref(A_Reg(A_RegName.RetReg), A_RegDeref(A_MemOffset(A_Reg(A_RegName.RetReg), A_OffsetImm(ZERO_IMM))), sizeOf(ty))
        case _ =>
            // Either T_ArrayElem or T_PairElem
            // we assume the value in RetReg is a pointer to the element and deref this value to get value stored
        
            builder += A_MovFromDeref(A_Reg(A_RegName.RetReg), A_RegDeref(A_MemOffset(A_Reg(A_RegName.RetReg), A_OffsetImm(ZERO_IMM))), PTR_SIZE)

    builder.toList

private def genFuncCall(v: Name, args: List[T_Expr], stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = {
    val builder = new ListBuffer[A_Instr]

    val funcStackTable = ctx.stackTables.funcTables(v)

    builder += A_Sub(A_Reg(A_RegName.StackPtr), A_Imm(funcStackTable.paramsSize), PTR_SIZE)

    val names = ctx.typeInfo.funcTys(v)._2
    
    (names.zip(args)).zip(funcStackTable.putDownInstrs(names)).foreach(
        (namesExprs, instr) => {
            builder ++= gen(namesExprs._2, stackTable)
            builder += instr
        }
    )

    builder += A_Call(funcLabelGen(v))

    builder += A_Add(A_Reg(A_RegName.StackPtr), A_Imm(funcStackTable.paramsSize), PTR_SIZE)

    builder.toList
}

private def genArrayLiteral(xs: List[T_Expr], ty: SemType, length: Int, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]
    val sizeBytes = opSizeToInt(INT_SIZE) + (intSizeOf(ty) * length)

    builder += A_MovTo(A_Reg(A_RegName.R1), A_Imm(sizeBytes), INT_SIZE)
    
    ctx.addDefaultFunc(MALLOC_LABEL)

    builder += A_Call(A_InstrLabel(MALLOC_LABEL))
    builder += A_MovTo(A_Reg(A_RegName.R11), A_Reg(A_RegName.RetReg), PTR_SIZE)
    builder += A_Add(A_Reg(A_RegName.R11), A_Imm(opSizeToInt(INT_SIZE)), PTR_SIZE)
    builder += A_MovDeref(A_RegDeref(A_MemOffset(A_Reg(A_RegName.R11), A_OffsetImm(-opSizeToInt(INT_SIZE)))), A_Imm(length), INT_SIZE)

    for (i <- 0 to length - 1) { 
        builder ++= gen(xs(i), stackTable)
        builder += A_MovDeref(A_RegDeref(A_MemOffset(A_Reg(A_RegName.R11), A_OffsetImm(i * intSizeOf(ty)))), A_Reg(A_RegName.RetReg), sizeOf(ty))
    }

    builder += A_MovTo(A_Reg(A_RegName.RetReg), A_Reg(A_RegName.R11), PTR_SIZE)

    builder.toList

private def genNewPair(x1: T_Expr, x2: T_Expr, ty1: SemType, ty2: SemType, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    builder += A_MovTo(A_Reg(A_RegName.R1), A_Imm(opSizeToInt(PTR_SIZE) * 2), INT_SIZE)
    
    ctx.addDefaultFunc(MALLOC_LABEL)

    builder += A_Call(A_InstrLabel(MALLOC_LABEL))

    builder += A_MovTo(A_Reg(A_RegName.R11), A_Reg(A_RegName.RetReg), PTR_SIZE)
    builder ++= gen(x1, stackTable)
    builder += A_MovDeref(A_RegDeref(A_MemOffset(A_Reg(A_RegName.R11), A_OffsetImm(ZERO_IMM))), A_Reg(A_RegName.RetReg), sizeOf(ty1))
    builder ++= gen(x2, stackTable)
    builder += A_MovDeref(A_RegDeref(A_MemOffset(A_Reg(A_RegName.R11), A_OffsetImm(PAIR_OFFSET_SIZE))), A_Reg(A_RegName.RetReg), sizeOf(ty2))
    builder += A_MovTo(A_Reg(A_RegName.RetReg), A_Reg(A_RegName.R11), PTR_SIZE)

    builder.toList

private def funcLabelGen(f: Name): A_InstrLabel = A_InstrLabel(s".F.${f.name}")

inline def sizeOf(ty: SemType): A_OperandSize = ty match
    case ? => PTR_SIZE
    case X => throw Exception("Should not have semType X in codeGen")
    case wacc.KnownType.Int => INT_SIZE
    case wacc.KnownType.Boolean => BOOL_SIZE
    case wacc.KnownType.Char => CHAR_SIZE
    case wacc.KnownType.String => PTR_SIZE
    case wacc.KnownType.Array(ty) => PTR_SIZE
    case KnownType.Pair(_, _) => PTR_SIZE
    case KnownType.Ident => ???

inline def typeToLetter(ty: SemType): String = ty match
    case ? => throw Exception("Should not have semType ? in codeGen")
    case X => throw Exception("Should not have semType ? in codeGen")
    case wacc.KnownType.Int => "i"
    case wacc.KnownType.Boolean => "b"
    case wacc.KnownType.Char => "c"
    case wacc.KnownType.String => "s"
    case wacc.KnownType.Array(wacc.KnownType.Char) => "s"
    case wacc.KnownType.Array(ty) => "p"
    case KnownType.Pair(ty1, ty2) => "p"
    case KnownType.Ident => ???

inline def opSizeToInt(opSize: A_OperandSize): Int = opSize match
    case A_OperandSize.A_8 => 1
    case A_OperandSize.A_16 => 2
    case A_OperandSize.A_32 => 4
    case A_OperandSize.A_64 => 8

inline def intSizeOf(ty: SemType): Int = opSizeToInt(sizeOf(ty)) 