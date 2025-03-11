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

def genFuncCall(v: Name, args: List[T_Expr], stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = {
    val builder = new ListBuffer[A_Instr]
    val funcStackTable = ctx.stackTables.funcTables(v)
    val names = ctx.typeInfo.funcTys(v)._2

    builder += A_Sub(A_Reg(A_RegName.StackPtr), A_Imm(funcStackTable.paramsSize), PTR_SIZE)

    (names.zip(args)).zip(funcStackTable.argStoreInstrs(names)).foreach {
        (namesExprs, instr) => {
            builder ++= gen(namesExprs._2, stackTable)
            builder += instr
        }
    }

    builder += A_Call(funcLabelGen(v))
    builder += A_Add(A_Reg(A_RegName.StackPtr), A_Imm(funcStackTable.paramsSize), PTR_SIZE)

    builder.toList
}

private def genDecl(v: Name, r: T_RValue, ty: SemType, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = 
    genAsgn(T_Ident(v), r, ty, stackTable)

private def genAsgn(l: T_LValue, r: T_RValue, ty: SemType, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(r, stackTable)

    l match
        case T_Ident(v) =>
            builder ++= stackTable.set(v)
        case T_ArrayElem(v, indices) => 
            val ty = unwrapArrType(ctx.typeInfo.varTys(v), indices.length)

            builder += A_Push(A_Reg(A_RegName.RetReg))
            builder ++= genIdent(v, stackTable)

            for i <- 0 to indices.length - 2 do
                builder += A_Push(A_Reg(A_RegName.RetReg))
                builder += A_Mov(A_Reg(A_RegName.RetReg), A_Imm(ZERO_IMM), PTR_SIZE)
                builder ++= gen(indices(i), stackTable)
                builder ++= indexArray(opSizeToInt(PTR_SIZE))
                builder += A_Mov(A_Reg(A_RegName.RetReg), A_RegDeref(A_MemOffset(A_Reg(A_RegName.RetReg), NO_OFFSET)), PTR_SIZE)

            builder += A_Push(A_Reg(A_RegName.RetReg))
            builder += A_Mov(A_Reg(A_RegName.RetReg), A_Imm(ZERO_IMM), PTR_SIZE)
            builder ++= gen(indices(indices.length - 1), stackTable)
            builder ++= indexArray(opSizeToInt(sizeOf(ty)))
            builder += A_Pop(A_Reg(A_RegName.Arg1))
            builder += A_Mov(A_MemOffset(A_Reg(A_RegName.RetReg), NO_OFFSET), A_Reg(A_RegName.Arg1), sizeOf(ty))

        case T_PairElem(index, v) =>
            builder += A_Push(A_Reg(A_RegName.RetReg))
            builder ++= getPairElemPtr(index, v, stackTable)
            builder += A_Pop(A_Reg(A_RegName.Arg1))
            builder += A_Mov(A_RegDeref(A_MemOffset(A_Reg(A_RegName.RetReg), NO_OFFSET)), A_Reg(A_RegName.Arg1), PTR_SIZE)
    
    builder.toList

private def genRead(l: T_LValue, ty: SemType, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    /* load current value of lvalue into register */
    builder ++= gen(l, stackTable)
    builder += A_Mov(A_Reg(A_RegName.Arg1), A_Reg(A_RegName.RetReg), sizeOf(ty))

    if ty == KnownType.Int then
        ctx.addDefaultFunc(READI_LABEL)
        builder += A_Call(READI_LABEL)
    else if ty == KnownType.Char then
        ctx.addDefaultFunc(READC_LABEL)
        builder += A_Call(READC_LABEL)
    else
        throw new Exception("Invalid type for read (should be caught in type checker)")

    /* move eax value back into lvalue.
    NOTE: we can still assume value is in ret reg */
    l match
        case T_Ident(v) =>
            builder ++= stackTable.set(v)
        case T_ArrayElem(v, indices) => 
            builder += A_Push(A_Reg(A_RegName.RetReg))
            builder ++= getPointerToArrayElem(v, indices, stackTable)
            builder += A_Pop(A_Reg(A_RegName.Arg1))
            builder += A_Mov(
                A_RegDeref(A_MemOffset(A_Reg(A_RegName.RetReg), NO_OFFSET)), 
                A_Reg(A_RegName.Arg1),
                sizeOf(ty))

        case T_PairElem(index, T_Ident(v)) => 
            val offset = index match
                case PairIndex.First  => ZERO_IMM
                case PairIndex.Second => opSizeToInt(PTR_SIZE)
            
            builder += A_Push(A_Reg(A_RegName.RetReg))
            builder ++= stackTable.get(v)
            builder += A_Pop(A_Reg(A_RegName.Arg1))
            builder += A_Mov(
                A_RegDeref(A_MemOffset(A_Reg(A_RegName.RetReg), A_OffsetImm(offset))), 
                A_Reg(A_RegName.Arg1), 
                sizeOf(ty))
        
        case T_PairElem(index, T_ArrayElem(v, indices)) =>
            builder += A_Push(A_Reg(A_RegName.RetReg))
            genArrayElem(v, indices, stackTable)
            builder += A_Pop(A_Reg(A_RegName.Arg1))

        case T_PairElem(_, _) => 
            throw IllegalArgumentException("Can't read from nested pairs. Should be caught in type checker")

    builder.toList

private def genFree(x: T_Expr, ty: SemType, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(x, stackTable)
    builder += A_Mov(A_Reg(A_RegName.Arg1), TEMP_REG, PTR_SIZE)
    
    ty match
        case KnownType.Array(_) =>
            ctx.addDefaultFunc(FREE_LABEL)
            builder += A_Sub(A_Reg(A_RegName.Arg1), A_Imm(opSizeToInt(INT_SIZE)), PTR_SIZE)
            builder += A_Call(FREE_LABEL)

        case KnownType.Pair(_, _) =>
            ctx.addDefaultFunc(ERR_NULL_PAIR_LABEL)
            ctx.addDefaultFunc(FREE_PAIR_LABEL)
            builder += A_Cmp(A_Reg(A_RegName.Arg1), A_Imm(ZERO_IMM), PTR_SIZE)
            builder += A_Jmp(ERR_NULL_PAIR_LABEL, A_Cond.Eq)
            builder += A_Call(FREE_PAIR_LABEL)

        case _ => throw Exception("Invalid type with free. Should be caught in type checker!")
    
    builder.toList

private def genReturn(x: T_Expr, ty: SemType, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder: ListBuffer[A_Instr] = ListBuffer()

    builder ++= gen(x, stackTable)
    builder += A_Add(A_Reg(A_RegName.StackPtr), A_Imm(stackTable.scopeSize), PTR_SIZE)
    builder += A_Pop(A_Reg(A_RegName.BasePtr))
    builder += A_Ret

    builder.toList

/**
 * assumes x is an integer
 */
private def genExit(x: T_Expr, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = 
    val builder = new ListBuffer[A_Instr]

    ctx.addDefaultFunc(EXIT_LABEL)

    builder ++= gen(x, stackTable)
    builder += A_Mov(A_Reg(A_RegName.Arg1), A_Reg(A_RegName.RetReg), INT_SIZE)
    builder += A_Call(EXIT_LABEL)

    builder.toList

private def genPrint(x: T_Expr, ty: SemType, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    ty match
        case KnownType.Int                   => ctx.addDefaultFunc(PRINTI_LABEL)
        case KnownType.Boolean               => ctx.addDefaultFunc(PRINTB_LABEL)
        case KnownType.Char                  => ctx.addDefaultFunc(PRINTC_LABEL)
        case KnownType.String                => ctx.addDefaultFunc(PRINTS_LABEL)
        case KnownType.Array(KnownType.Char) => ctx.addDefaultFunc(PRINTS_LABEL)
        case _                               => ctx.addDefaultFunc(PRINTP_LABEL)

    builder ++= gen(x, stackTable)
    builder += A_Mov(A_Reg(A_RegName.Arg1), A_Reg(A_RegName.RetReg), sizeOf(ty))
    builder += A_Call(A_InstrLabel(s"_print${{typeToLetter(ty)}}"))

    builder.toList

private def genPrintln(x: T_Expr, ty: SemType, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    ctx.addDefaultFunc(PRINTLN_LABEL)

    builder ++= genPrint(x, ty, stackTable)
    builder += A_Call(PRINTLN_LABEL)

    builder.toList

private def genIfHelper(cond: T_Expr, body: List[T_Stmt], scopedBody: Set[Name], el: List[T_Stmt], scopedEl: Set[Name], stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]
    val bodyLabel = ctx.genNextInstrLabel() // .L0
    val restLabel = ctx.genNextInstrLabel() // .L1

    builder ++= gen(cond, stackTable) // condition 
    builder += A_Cmp(A_Reg(A_RegName.RetReg), A_Imm(TRUE), BOOL_SIZE)
    builder += A_Jmp(bodyLabel, A_Cond.Eq)
    builder ++= genCodeBlock(el, scopedEl, stackTable)  // else block 
    builder += A_Jmp(restLabel, A_Cond.Uncond) 
    builder += A_LabelStart(bodyLabel) // body block 
    builder ++= genCodeBlock(body, scopedBody, stackTable)
    builder += A_LabelStart(restLabel)

    builder.toList

private def genIf(cond: T_Expr, body: List[T_Stmt], scopedBody: Set[Name], el: List[T_Stmt], scopedEl: Set[Name], stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = 
    val builder = new ListBuffer[A_Instr]

    cond match 
        case _: T_GreaterThan | 
             _: T_GreaterThanEq | 
             _: T_LessThan | 
             _: T_LessThanEq | 
             _: T_Eq | 
             _: T_NotEq | 
             _: T_And | 
             _: T_Or | 
             _: T_Not | 
             _: T_BoolLiteral | 
             _: T_Ident => ()
        case _ => throw new Exception(s"Should not reach here. Got $cond")
    
    builder ++= genIfHelper(cond, body, scopedBody, el, scopedEl, stackTable)

    builder.toList

private def genWhile(cond: T_Expr, body: List[T_Stmt], scoped: Set[Name], stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]
    val condLabel = ctx.genNextInstrLabel() // .L0
    val bodyLabel = ctx.genNextInstrLabel() // .L1

    builder += A_Jmp(condLabel, A_Cond.Uncond)
    builder += A_LabelStart(bodyLabel)
    builder ++= genCodeBlock(body, scoped, stackTable)  
    builder += A_LabelStart(condLabel)
    builder ++= gen(cond, stackTable)
    builder += A_Cmp(A_Reg(A_RegName.RetReg), A_Imm(TRUE), BOOL_SIZE)
    builder += A_Jmp(bodyLabel, A_Cond.Eq)

    builder.toList

private def genCodeBlock(body: List[T_Stmt], scoped: Set[Name], stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = 
    body.flatMap(gen(_, stackTable))

private def genSkip(): List[A_Instr] = List()
