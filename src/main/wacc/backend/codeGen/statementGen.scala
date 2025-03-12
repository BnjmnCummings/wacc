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
 * Generates the assembly representation of a typed program.
 * @param funcName the name of the function to be called.
 * @param args the expressions to be 'passed in' as each argument.
 * @return a list of assembly instructions representing the function call.
 */
def genFuncCall(funcName: Name, args: List[T_Expr], stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = 
    val builder = new ListBuffer[A_Instr]
    val funcStackTable = ctx.stackTables.funcTables(funcName)
    val argNames = ctx.typeInfo.funcTys(funcName)._2

    builder += A_Sub(A_Reg(A_RegName.StackPtr), A_Imm(funcStackTable.paramsSize), PTR_SIZE)

    (argNames.zip(args)).zip(funcStackTable.argStoreInstrs(argNames)).foreach {  // TODO: Refactor
        (namesExprs, instr) => {
            builder ++= gen(namesExprs._2, stackTable)
            builder += instr
        }
    }

    builder += A_Call(funcLabelGen(funcName))
    builder += A_Add(A_Reg(A_RegName.StackPtr), A_Imm(funcStackTable.paramsSize), PTR_SIZE)

    builder.toList

/**
  * Generates the assembly representation of an assignment.
  * @param lvalue the left hand side of the assignment.
  * @param rvalue the right hand side of the assignment.
  * @param ty the type of the assignment.
  * @param stackTable the stack table of the current scope.
  * @return a list of assembly instructions representing the assignment.
  */
def genAsgn(lvalue: T_LValue, rvalue: T_RValue, ty: SemType, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(rvalue, stackTable)

    lvalue match
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
                builder ++= indexArray(numOfBytes(PTR_SIZE))
                builder += A_Mov(A_Reg(A_RegName.RetReg), A_RegDeref(A_MemOffset(A_Reg(A_RegName.RetReg), NO_OFFSET)), PTR_SIZE)

            builder += A_Push(A_Reg(A_RegName.RetReg))
            builder += A_Mov(A_Reg(A_RegName.RetReg), A_Imm(ZERO_IMM), PTR_SIZE)
            builder ++= gen(indices(indices.length - 1), stackTable)
            builder ++= indexArray(numOfBytes(sizeOf(ty)))
            builder += A_Pop(A_Reg(A_RegName.Arg1))
            builder += A_Mov(A_MemOffset(A_Reg(A_RegName.RetReg), NO_OFFSET), A_Reg(A_RegName.Arg1), sizeOf(ty))

        case T_PairElem(index, v) =>
            builder += A_Push(A_Reg(A_RegName.RetReg))
            builder ++= getPairElemPtr(index, v, stackTable)
            builder += A_Pop(A_Reg(A_RegName.Arg1))
            builder += A_Mov(A_RegDeref(A_MemOffset(A_Reg(A_RegName.RetReg), NO_OFFSET)), A_Reg(A_RegName.Arg1), PTR_SIZE)
    
    builder.toList

/**
  * Generates the assembly representation of a variable declaration.
  * @param v the name of the variable.
  * @param rvalue the right hand side of the declaration.
  * @param ty the type of the new variable.
  * @param stackTable the stack table of the current scope.
  * @return a list of assembly instructions representing the declaration.
  */
def genDecl(v: Name, rvalue: T_RValue, ty: SemType, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = 
    genAsgn(T_Ident(v), rvalue, ty, stackTable)

/**
  * Generates the assembly representation of a read statement.
  * @param l the lvalue to read into.
  * @param ty the type of the lvalue.
  * @param stackTable the stack table of the current scope.
  * @return a list of assembly instructions representing the read statement.
  */
def genRead(l: T_LValue, ty: SemType, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
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
                case PairIndex.Second => numOfBytes(PTR_SIZE)
            
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

/**
  * Generates the assembly representation of a free statement.
  * @param expr the expression to be freed.
  * @param ty the type of the expression.
  * @param stackTable the stack table of the current scope.
  * @return a list of assembly instructions representing the free statement.
  */
def genFree(expr: T_Expr, ty: SemType, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    builder ++= gen(expr, stackTable)
    builder += A_Mov(A_Reg(A_RegName.Arg1), TEMP_REG, PTR_SIZE)
    
    ty match
        case KnownType.Array(_) =>
            ctx.addDefaultFunc(FREE_LABEL)
            builder += A_Sub(A_Reg(A_RegName.Arg1), A_Imm(numOfBytes(INT_SIZE)), PTR_SIZE)
            builder += A_Call(FREE_LABEL)

        case KnownType.Pair(_, _) =>
            ctx.addDefaultFunc(ERR_NULL_PAIR_LABEL)
            ctx.addDefaultFunc(FREE_PAIR_LABEL)
            builder += A_Cmp(A_Reg(A_RegName.Arg1), A_Imm(ZERO_IMM), PTR_SIZE)
            builder += A_Jmp(ERR_NULL_PAIR_LABEL, A_Cond.Eq)
            builder += A_Call(FREE_PAIR_LABEL)

        case _ => throw Exception("Invalid type with free. Should be caught in type checker!")
    
    builder.toList

/**
 * Generates the assembly representation of a return statement.
 * @param expr the expression to be returned.
 * @param ty the type of the expression.
 * @param stackTable the stack table of the current scope.
 * @return a list of assembly instructions representing the return statement.
 */
def genReturn(expr: T_Expr, ty: SemType, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder: ListBuffer[A_Instr] = ListBuffer()

    builder ++= gen(expr, stackTable)
    builder += A_Add(A_Reg(A_RegName.StackPtr), A_Imm(stackTable.scopeSize), PTR_SIZE)
    builder += A_Pop(A_Reg(A_RegName.BasePtr))
    builder += A_Ret

    builder.toList

/**
 * Generates the assembly representation of an exit statement.
 * @param expr the expression to be returned.
 * @param stackTable the stack table of the current scope.
 * @return a list of assembly instructions representing the exit statement.
 */
def genExit(x: T_Expr, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = 
    val builder = new ListBuffer[A_Instr]

    ctx.addDefaultFunc(EXIT_LABEL)

    builder ++= gen(x, stackTable)
    builder += A_Mov(A_Reg(A_RegName.Arg1), A_Reg(A_RegName.RetReg), INT_SIZE)
    builder += A_Call(EXIT_LABEL)

    builder.toList

/**
 * Generates the assembly representation of a print statement.
 * @param expr the expression to be printed.
 * @param ty the type of the expression.
 * @param stackTable the stack table of the current scope.
 * @return a list of assembly instructions representing the print statement.
 */
def genPrint(expr: T_Expr, ty: SemType, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    ty match
        case KnownType.Int                   => ctx.addDefaultFunc(PRINTI_LABEL)
        case KnownType.Boolean               => ctx.addDefaultFunc(PRINTB_LABEL)
        case KnownType.Char                  => ctx.addDefaultFunc(PRINTC_LABEL)
        case KnownType.String                => ctx.addDefaultFunc(PRINTS_LABEL)
        case KnownType.Array(KnownType.Char) => ctx.addDefaultFunc(PRINTS_LABEL)
        case _                               => ctx.addDefaultFunc(PRINTP_LABEL)

    builder ++= gen(expr, stackTable)
    builder += A_Mov(A_Reg(A_RegName.Arg1), A_Reg(A_RegName.RetReg), sizeOf(ty))
    builder += A_Call(A_InstrLabel(s"_print${{typeToLetter(ty)}}"))

    builder.toList

/**
  * A helper function that maps eachtype to a letter.
  * Used to call the desired print function.
  * @param ty the type.
  * @return the respective letter.
  */
private inline def typeToLetter(ty: SemType): String = ty match
    case KnownType.Int                   => "i"
    case KnownType.Boolean               => "b"
    case KnownType.Char                  => "c"
    case KnownType.String                => "s"
    case KnownType.Array(KnownType.Char) => "s"
    case KnownType.Array(ty)             => "p"
    case KnownType.Pair(ty1, ty2)        => "p"
    case KnownType.Ident                 => throw Exception("Should get type info from context")
    case ?                               => throw Exception("Should not have semType ? in codeGen")
    case X                               => throw Exception("Should not have semType ? in codeGen")

/**
  * Generates the assembly representation of a println statement.
  * @param expr the expression to be printed.
  * @param ty the type of the expression.
  * @param stackTable the stack table of the current scope.
  * @return a list of assembly instructions representing the println statement.
  */
def genPrintln(expr: T_Expr, ty: SemType, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
    val builder = new ListBuffer[A_Instr]

    ctx.addDefaultFunc(PRINTLN_LABEL)

    builder ++= genPrint(expr, ty, stackTable)
    builder += A_Call(PRINTLN_LABEL)

    builder.toList

/**
  * Generates the assembly representation of a sequence of statements.
  * @param cond the condition of the if statement.
  * @param body the body of the if statement.
  * @param scopedBody the variables in the if scope.
  * @param el the else block of the if statement.
  * @param scopedEl the variables in the else scope.
  * @param stackTable the stack table of the current scope.
  * @return
  */
def genIf(cond: T_Expr, body: List[T_Stmt], scopedBody: Set[Name], el: List[T_Stmt], scopedEl: Set[Name], stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = 
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

// TODO: get rid of this
def genIfHelper(cond: T_Expr, body: List[T_Stmt], scopedBody: Set[Name], el: List[T_Stmt], scopedEl: Set[Name], stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
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

/**
  * Generates the assembly representation of a while loop.
  * @param cond the condition of the while loop.
  * @param body the body of the while loop.
  * @param scoped the variables in the while scope.
  * @param stackTable the stack table of the current scope.
  * @return
  */
def genWhile(cond: T_Expr, body: List[T_Stmt], scoped: Set[Name], stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] =
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
/**
  * Generates the assembly representation of a Begin/End block.
  * @param body
  * @param scoped
  * @param stackTable
  * @return
  */
def genCodeBlock(body: List[T_Stmt], scoped: Set[Name], stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = 
    body.flatMap(gen(_, stackTable))

/**
  * Generates the assembly representation of a Skip statement.
  * @return an empty list of assembly instructions.
  */
def genSkip(): List[A_Instr] = List()
