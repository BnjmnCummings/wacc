package wacc.codeGen

import wacc.t_ast.*
import wacc.assemblyIR.*
import wacc.TypeInfo
import wacc.ast.PairIndex

import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import wacc.parser.bool
import wacc.SemType

val TRUE = 1
val FALSE = 0
val ZERO_IMM = 0
val CHR_MASK = -128

class CodeGen(t_tree: T_Prog, typeInfo: TypeInfo) {
    private val storedStrings: mutable.Set[A_StoredStr] = mutable.Set()

    // TODO @Jack : Create a function that maps a KnownType to a size - this is useful for things like read (char/int)

    def generate(): A_Prog = ???

    private def generate(t: T_Stmt): List[A_Instr] = t match
        case T_Decl(v, r, ty) => generateDecl(v, r, ty)
        case T_Asgn(l, r, ty) => generateAsgn(l, r, ty)
        case T_Read(l, ty) => generateRead(l, ty)
        case T_Free(x, ty) => generateFree(x, ty)
        case T_Return(x, ty) => generateReturn(x, ty)
        case T_Exit(x) => generateExit(x)
        case T_Print(x, ty) => generatePrint(x, ty)
        case T_Println(x, ty) => generatePrintln(x, ty)
        case T_If(cond, body, scopedBody, el, scopedEl) => generateIf(cond, body, scopedBody, el, scopedEl)
        case T_While(cond, body, scoped) => generateWhile(cond, body, scoped)
        case T_CodeBlock(body, scoped) => generateCodeBlock(body, scoped)
        case T_Skip() => generateSkip()

    private def generate(t: T_Expr): List[A_Instr] = t match
        case T_Mul(x, y) => generateMul(x, y)
        case T_Div(x, y) => generateDiv(x, y)
        case T_Mod(x, y) => generateMod(x, y)
        case T_Add(x, y) => generateAdd(x, y)
        case T_Sub(x, y) => generateSub(x, y)
        case T_GreaterThan(x, y) => generateGreaterThan(x, y)
        case T_GreaterThanEq(x, y) => generateGreaterThanEq(x, y)
        case T_LessThan(x, y) => generateLessThan(x, y)
        case T_LessThanEq(x, y) => generateLessThanEq(x, y)
        case T_Eq(x, y) => generateEq(x, y)
        case T_NotEq(x, y) => generateNotEq(x, y)
        case T_And(x, y) => generateAnd(x, y)
        case T_Or(x, y) => generateOr(x, y)
        case T_Not(x) => generateNot(x)
        case T_Neg(x) => generateNeg(x)
        case T_Len(x) => generateLen(x)
        case T_Ord(x) => generateOrd(x)
        case T_Chr(x) => generateChr(x)
        case T_IntLiteral(v) => generateIntLiteral(v)
        case T_BoolLiteral(v) => generateBoolLiteral(v)
        case T_CharLiteral(v) => generateCharLiteral(v)
        case T_StringLiteral(v) => generateStringLiteral(v)
        case T_Ident(v) => generateIdent(v)
        case T_ArrayElem(v, indices) => generateArrayElem(v, indices)
        case T_PairNullLiteral => generatePairNullLiteral()
        case T_PairElem(index, v) => generatePairElem(index, v)
    
    private def generate(t: T_LValue) = t match
        case T_Ident(v) => generateIdent(v)
        case T_ArrayElem(v, indices) => generateArrayElem(v, indices)
        case T_PairElem(index, v) => generatePairElem(index, v)

    private def generate(t: T_RValue) = t match
        case T_FuncCall(v, args) => generateFuncCall(v, args)
        case T_ArrayLiteral(xs, ty, length) => generateArrayLiteral(xs, ty, length)
        case T_NewPair(x1, x2, ty1, ty2) => generateNewPair(x1, x2, ty1, ty2)

    private def generate(t: T_Func): A_Func = ???

    private def generateDecl(v: T_Name, r: T_RValue, ty: SemType): List[A_Instr] = ???

    private def generateAsgn(l: T_LValue, r: T_RValue, ty: SemType): List[A_Instr] = ???

    private def generateRead(l: T_LValue, ty: SemType): List[A_Instr] = ???

    private def generateFree(x: T_Expr, ty: SemType): List[A_Instr] = ???

    private def generateReturn(x: T_Expr, ty: SemType): List[A_Instr] = ???

    private def generateExit(x: T_Expr): List[A_Instr] = ???

    private def generatePrint(x: T_Expr, ty: SemType): List[A_Instr] = ???

    private def generatePrintln(x: T_Expr, ty: SemType): List[A_Instr] = ???

    private def generateIf(cond: T_Expr, body: List[T_Stmt], scopedBody: Set[T_Name], el: List[T_Stmt], scopedEl: Set[T_Name]): List[A_Instr] = ???

    private def generateWhile(cond: T_Expr, body: List[T_Stmt], scoped: Set[T_Name]): List[A_Instr] = ???

    private def generateCodeBlock(body: List[T_Stmt], scoped: Set[T_Name]): List[A_Instr] = ???

    private def generateSkip(): List[A_Instr] = List()

    private def generateMul(x: T_Expr, y: T_Expr): List[A_Instr] =
        val builder = new ListBuffer[A_Instr]

        builder ++= generate(x)
        builder += A_Push(A_Reg(intSize, A_RegName.RetReg))
        builder ++= generate(y)
        builder += A_Pop(A_Reg(intSize, A_RegName.R1))
        builder += A_IMul(A_Reg(intSize, A_RegName.RetReg), A_Reg(intSize, A_RegName.RetReg), A_Reg(intSize, A_RegName.R1), intSize)
        // TODO @Aidan: Overflow can occur here - add flag system etc.
        builder += A_Push(A_Reg(intSize, A_RegName.RetReg))

        builder.toList

    private def generateDivMod(x: T_Expr, y: T_Expr, divResultReg: A_RegName): List[A_Instr] =
        val builder = new ListBuffer[A_Instr]

        // Note: With IDiv, we need the numerator to be stored in eax and then we can divide by a given register
        // Note: IDiv stores remainder in edx(32bit) - R3

        builder ++= generate(x)
        builder += A_Push(A_Reg(intSize, A_RegName.RetReg))
        builder ++= generate(y)
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

        builder += A_Push(A_Reg(intSize, divResultReg))

        builder.toList

    private def generateDiv(x: T_Expr, y: T_Expr): List[A_Instr] = generateDivMod(x, y, A_RegName.RetReg)

    private def generateMod(x: T_Expr, y: T_Expr): List[A_Instr] = generateDivMod(x, y, A_RegName.R3)

    private def generateAddSub(x: T_Expr, y: T_Expr, instrApply: ((A_Reg, A_Operand, A_OperandSize) => A_Instr)) =
        val builder = new ListBuffer[A_Instr]

        builder ++= generate(x)
        builder += A_Push(A_Reg(intSize, A_RegName.RetReg))
        builder ++= generate(y)
        builder += A_Pop(A_Reg(intSize, A_RegName.R1))
        builder += instrApply(A_Reg(intSize, A_RegName.RetReg), A_Reg(intSize, A_RegName.R1), intSize)
        builder += A_Push(A_Reg(intSize, A_RegName.RetReg))
        // TODO @Aidan: Overflow can occur here - add flag system etc.

        builder.toList

    private def generateAdd(x: T_Expr, y: T_Expr): List[A_Instr] = generateAddSub(x, y, A_Add.apply)

    private def generateSub(x: T_Expr, y: T_Expr): List[A_Instr] = generateAddSub(x, y, A_Sub.apply)


    // TODO @Jack : Merge type changes into this branch and do the following:
    private def generateGreaterThan(x: T_Expr, y: T_Expr): List[A_Instr] = ???

    private def generateGreaterThanEq(x: T_Expr, y: T_Expr): List[A_Instr] = ???

    private def generateLessThan(x: T_Expr, y: T_Expr): List[A_Instr] = ???

    private def generateLessThanEq(x: T_Expr, y: T_Expr): List[A_Instr] = ???

    private def generateEq(x: T_Expr, y: T_Expr): List[A_Instr] = ???

    private def generateNotEq(x: T_Expr, y: T_Expr): List[A_Instr] = ???

    private def generateBitwiseOp(x: T_Expr, y: T_Expr, instrApply: ((A_Reg, A_Operand, A_OperandSize) => A_Instr)): List[A_Instr] =
        val builder = new ListBuffer[A_Instr]

        builder ++= generate(x)
        builder += A_Push(A_Reg(boolSize, A_RegName.RetReg))
        builder ++= generate(y)
        builder += A_Pop(A_Reg(boolSize, A_RegName.R1))
        builder += instrApply(A_Reg(boolSize, A_RegName.RetReg), A_Reg(boolSize, A_RegName.R1), boolSize)
        builder += A_Push(A_Reg(boolSize, A_RegName.RetReg))

        builder.toList

    private def generateAnd(x: T_Expr, y: T_Expr): List[A_Instr] = generateBitwiseOp(x, y, A_And.apply)

    private def generateOr(x: T_Expr, y: T_Expr): List[A_Instr] = generateBitwiseOp(x, y, A_Or.apply)

    private def generateNot(x: T_Expr): List[A_Instr] = 
        val builder = new ListBuffer[A_Instr]

        builder ++= generate(x)
        builder += A_Xor(A_Reg(boolSize, A_RegName.RetReg), A_Imm(TRUE), boolSize)
        builder += A_Push(A_Reg(boolSize, A_RegName.RetReg))

        builder.toList

    private def generateNeg(x: T_Expr): List[A_Instr] =
        val builder = new ListBuffer[A_Instr]

        builder ++= generate(x)

        // CONSIDER: DO WE NEED TO SAVE R1 BEFORE THIS?
        builder += A_Mov(A_Reg(intSize, A_RegName.R1), A_Imm(ZERO_IMM))
        builder += A_Sub(A_Reg(intSize, A_RegName.R1), A_Reg(intSize, A_RegName.RetReg), intSize)
        // check overflow -2^32 case! TODO @Aidan
        builder += A_Push(A_Reg(intSize, A_RegName.RetReg))

        builder.toList

    private def generateLen(x: T_Expr): List[A_Instr] = ???

    private def generateOrd(x: T_Expr): List[A_Instr] = 
        val builder = new ListBuffer[A_Instr]

        builder ++= generate(x)
        builder += A_Movzx(A_Reg(intSize, A_RegName.R1), A_Reg(charSize, A_RegName.RetReg))
        builder += A_Push(A_Reg(intSize, A_RegName.R1))

        builder.toList

    private def generateChr(x: T_Expr): List[A_Instr] = 
        val builder = new ListBuffer[A_Instr]

        builder ++= generate(x)

        builder += A_Mov(A_Reg(intSize, A_RegName.R1), A_Reg(intSize, A_RegName.RetReg))
        builder += A_And(A_Reg(intSize, A_RegName.R1), A_Imm(CHR_MASK), intSize)
        builder += A_Cmp(A_Reg(intSize, A_RegName.R1), A_Imm(ZERO_IMM), intSize)
        builder += A_Jmp(???, A_Cond.NEq)
        // TODO: @Aidan Create bad character label - this is when you chr(x) |x| > 127 (0b1111111)

        // Below: Rs has charSize as we only care about the 8 LSBs
        builder += A_Push(A_Reg(charSize, A_RegName.RetReg))

        builder.toList

    private def generateIntLiteral(v: BigInt): List[A_Instr] = List(A_Mov(A_Reg(intSize, A_RegName.RetReg), A_Imm(v)))

    private def generateBoolLiteral(v: Boolean): List[A_Instr] = List(A_Mov(A_Reg(boolSize, A_RegName.RetReg), A_Imm(if v then TRUE else FALSE)))

    private def generateCharLiteral(v: Char): List[A_Instr] = List(A_Mov(A_Reg(charSize, A_RegName.RetReg), A_Imm(v.toInt)))

    private def generateStringLiteral(v: String): List[A_Instr] = ???

    private def generateIdent(v: T_Name): List[A_Instr] = ???

    private def generateArrayElem(v: T_Name, indices: List[T_Expr]): List[A_Instr] = ???

    private def generatePairNullLiteral(): List[A_Instr] = ???

    private def generatePairElem(index: PairIndex, v: T_LValue): List[A_Instr] = ???

    private def generateFuncCall(v: T_Name, args: List[T_Expr]): List[A_Instr] = ???

    private def generateArrayLiteral(xs: List[T_Expr], ty: SemType, length: BigInt): List[A_Instr] = ???

    private def generateNewPair(x1: T_Expr, x2: T_Expr, ty1: SemType, ty2: SemType): List[A_Instr] = ???
}