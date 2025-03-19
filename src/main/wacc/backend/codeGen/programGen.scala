package wacc.codeGen

import wacc.t_ast.*
import wacc.q_ast.Name
import wacc.assemblyIR.*
import wacc.semantic.TypeInfo
import wacc.semantic.SemType
import wacc.semantic.?
import wacc.semantic.X
import wacc.semantic.KnownType
import wacc.EXIT_SUCCESS

import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.collection.immutable

/**
  * Generates the assembly representation of a typed program.
  * @param prog the typed program to be converted.
  * @param typeInfo the type information of the program.
  * @return the assembly representation of the program.
  */
def gen(prog: T_Prog, typeInfo: TypeInfo): A_Prog = 
    /* first pass of the program to pre-generate every scope/stacktable*/
    given ctx: CodeGenCtx = CodeGenCtx(typeInfo, getTables(prog, typeInfo))

    val funcs = prog.funcs.map(gen)
    val builder: ListBuffer[A_Instr] = ListBuffer()

    builder += A_Push(A_Reg(A_RegName.BasePtr))
    builder += A_Mov(A_Reg(A_RegName.BasePtr), A_Reg(A_RegName.StackPtr), PTR_SIZE)
    builder += A_Sub(A_Reg(A_RegName.StackPtr), A_Imm(ctx.stackTables.mainTable.scopeSize), PTR_SIZE)
    builder ++= prog.body.flatMap(gen(_, ctx.stackTables.mainTable))
    builder += A_Mov(A_Reg(A_RegName.RetReg), A_Imm(EXIT_SUCCESS), PTR_SIZE)
    builder += A_Add(A_Reg(A_RegName.StackPtr), A_Imm(ctx.stackTables.mainTable.scopeSize), PTR_SIZE)
    builder += A_Pop(A_Reg(A_RegName.BasePtr))
    builder += A_Ret

    val main = A_Func(A_DefaultLabel(MAIN_FUNC_NAME), builder.toList)
    val _funcsWithDefaults = funcs ++ ctx.getDefaultFuncs

    A_Prog(ctx.getStoredStrings, main :: _funcsWithDefaults)

/**
  * Generates the assembly instructions for a given statement.
  * @param stmt the typed statement to be converted.
  * @param stackTable a table containing all the variables in scope with their respective offsets.
  * @return the list of assembly instructions to represent that statement.
  */
private def gen(stmt: T_Stmt, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = stmt match
    case T_Decl(v, r, ty)                           => genDecl(v, r, ty, stackTable)
    case T_Asgn(l, r, ty)                           => genAsgn(l, r, ty, stackTable)
    case T_Read(l, ty)                              => genRead(l, ty, stackTable)
    case T_Free(x, ty)                              => genFree(x, ty, stackTable)
    case T_Return(x, ty)                            => genReturn(x, ty, stackTable)
    case T_Exit(x)                                  => genExit(x, stackTable)
    case T_Print(x, ty)                             => genPrint(x, ty, stackTable)
    case T_Println(x, ty)                           => genPrintln(x, ty, stackTable)
    case T_If(cond, body, scopedBody, el, scopedEl) => genIf(cond, body, scopedBody, el, scopedEl, stackTable)
    case T_While(cond, body, scoped)                => genWhile(cond, body, scoped, stackTable)
    case T_CodeBlock(body, scoped)                  => genCodeBlock(body, scoped, stackTable)
    case T_Skip()                                   => genSkip()

/**
  * Generates the assembly instructions for a given expression.
  * @param expr the typed expression to be converted.
  * @param stackTable a table containing all the variables in scope with their respective offsets.
  * @return the list of assembly instructions to represent that expression.
  */
private def gen(expr: T_Expr, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = expr match
    case T_Mul(x, y)               => genMul(x, y, stackTable)
    case T_Div(x, y)               => genDivMod(x, y, A_RegName.RetReg, stackTable)
    case T_Mod(x, y)               => genDivMod(x, y, A_RegName.Arg3, stackTable)
    case T_Add(x, y)               => genAddSub(x, y, A_Add.apply, stackTable)
    case T_Sub(x, y)               => genAddSub(x, y, A_Sub.apply, stackTable)
    case T_GreaterThan(x, y, ty)   => genComparison(x, y, ty, A_Cond.Gt, stackTable)
    case T_GreaterThanEq(x, y, ty) => genComparison(x, y, ty, A_Cond.GEq, stackTable)
    case T_LessThan(x, y, ty)      => genComparison(x, y, ty, A_Cond.Lt, stackTable)
    case T_LessThanEq(x, y, ty)    => genComparison(x, y, ty, A_Cond.LEq, stackTable)
    case T_Eq(x, y, ty)            => genComparison(x, y, ty, A_Cond.Eq, stackTable)
    case T_NotEq(x, y, ty)         => genComparison(x, y, ty, A_Cond.NEq, stackTable)
    case T_And(x, y)               => genBitwiseOp(x, y, stackTable, A_Cond.NEq)
    case T_Or(x, y)                => genBitwiseOp(x, y, stackTable, A_Cond.Eq)
    case T_Not(x)                  => genNot(x, stackTable)
    case T_Neg(x)                  => genNeg(x, stackTable)
    case T_Len(x)                  => genLen(x, stackTable)
    case T_Ord(x)                  => genOrd(x, stackTable)
    case T_Chr(x)                  => genChr(x, stackTable)
    case T_IntLiteral(v)           => genIntLiteral(v)
    case T_BoolLiteral(v)          => genBoolLiteral(v)
    case T_CharLiteral(v)          => genCharLiteral(v)
    case T_StringLiteral(v)        => genStringLiteral(v)
    case T_Ident(v)                => genIdent(v, stackTable)
    case T_ArrayElem(v, indices)   => genArrayElem(v, indices, stackTable)
    case T_PairNullLiteral         => genPairNullLiteral()
    case T_PairElem(index, v)      => genPairElem(index, v, stackTable)

/**
  * Generates the assembly instructions for a given l-value.
  * @param lvalue the typed l-value to be converted.
  * @param stackTable a table containing all the variables in scope with their respective offsets.
  * @return the list of assembly instructions to represent that l-value.
  */
private def gen(lvalue: T_LValue, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = lvalue match
    case T_Ident(v)              => genIdent(v, stackTable)
    case T_ArrayElem(v, indices) => genArrayElem(v, indices, stackTable)
    case T_PairElem(index, v)    => genPairElem(index, v, stackTable)

/**
  * Generates the assembly instructions for a given r-value.
  * @param rvalue the typed r-value to be converted.
  * @param stackTable a table containing all the variables in scope with their respective offsets.
  * @return the list of assembly instructions to represent that r-value.
  */
private def gen(rvalue: T_RValue, stackTable: StackTables)(using ctx: CodeGenCtx): List[A_Instr] = rvalue match
    case T_FuncCall(v, args)            => genFuncCall(v, args, stackTable)
    case T_ArrayLiteral(xs, ty, length) => genArrayLiteral(xs, ty, length, stackTable)
    case T_NewPair(x1, x2, ty1, ty2)    => genNewPair(x1, x2, ty1, ty2, stackTable)
    case expr: T_Expr                   => gen(expr, stackTable)

/**
  * Generates the assembly instructions for a given function.
  * @param func the typed function to be converted.
  * @param stackTable a table containing all the variables in scope with their respective offsets.
  * @return the list of assembly instructions to represent that function.
  */
private def gen(func: T_Func)(using ctx: CodeGenCtx): A_Func =
    val stackTable: StackTables = ctx.stackTables.funcTables(func.name)
    val builder: ListBuffer[A_Instr] = ListBuffer()

    builder += A_Push(A_Reg(A_RegName.BasePtr))
    builder += A_Mov(A_Reg(A_RegName.BasePtr), A_Reg(A_RegName.StackPtr), PTR_SIZE)
    builder += A_Sub(A_Reg(A_RegName.StackPtr), A_Imm(stackTable.scopeSize), PTR_SIZE)
    builder ++= func.body.flatMap(gen(_, stackTable))
    builder += A_Mov(A_Reg(A_RegName.RetReg), A_Imm(EXIT_SUCCESS), PTR_SIZE)
    builder += A_Add(A_Reg(A_RegName.StackPtr), A_Imm(stackTable.scopeSize), PTR_SIZE)
    builder += A_Pop(A_Reg(A_RegName.BasePtr))
    builder += A_Ret

    A_Func(funcLabelGen(func.name), builder.toList)

/**
  * A helper function that returns a unique subroutine lable.
  * It is assumed at this stage that all function names are unique.
  * @param funcName the name of the function.
  * @return a unique instruction label for the function.
  */
inline def funcLabelGen(funcName: Name): A_InstrLabel = 
    A_InstrLabel(s".F.${funcName.value}")


/**
  * A helper function that returns the 'size' of a type.
  * @param ty the type.
  * @return it's size as an [A_OperandSize].
  */
inline def sizeOf(ty: SemType): A_OperandSize = ty match
    case ?                    => PTR_SIZE // 
    case KnownType.Int        => INT_SIZE
    case KnownType.Boolean    => BOOL_SIZE
    case KnownType.Char       => CHAR_SIZE
    case KnownType.String     => PTR_SIZE
    case KnownType.Array(ty)  => PTR_SIZE
    case KnownType.Pair(_, _) => PTR_SIZE
    case X                    => throw Exception("Should not have semType X in codeGen")
    case KnownType.Ident      => throw Exception("Should get type info from context")

/**
  * A helper function that maps an [A_OperandSize] to its size in bytes.
  * @param opSize the size of the operand as an [A_OperandSize] enum.
  * @return the number of bytes it represents.
  */
inline def numOfBytes(opSize: A_OperandSize): Int = opSize match
    case A_OperandSize.A_8  => 1
    case A_OperandSize.A_16 => 2
    case A_OperandSize.A_32 => 4
    case A_OperandSize.A_64 => 8

/**
  * An overload of numOfBytes that takes in a [SemType].
  * @param ty the type as a [SemType].
  * @return the number of bytes it represents
  */
inline def numOfBytes(ty: SemType): Int = 
    numOfBytes(sizeOf(ty))
