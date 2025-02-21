package wacc.codeGen

import wacc.t_ast.*
import wacc.assemblyIR.*
import wacc.TypeInfo
import wacc.ast.PairIndex

import scala.collection.mutable.ListBuffer

class CodeGen(t_tree: T_Prog, typeInfo: TypeInfo) {
    private val storedStrings: ListBuffer[A_StoredStr] = ListBuffer()

    def generate(): A_Prog = ???

    private def generate(t: T_Stmt): List[A_Instr] = t match
        case T_Decl(v, r) => generateDecl(v, r)
        case T_Asgn(l, r) => generateAsgn(l, r)
        case T_Read(l) => generateRead(l)
        case T_Free(x) => generateFree(x)
        case T_Return(x) => generateReturn(x)
        case T_Exit(x) => generateExit(x)
        case T_Print(x) => generatePrint(x)
        case T_Println(x) => generatePrintln(x)
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
        case T_ArrayLiteral(xs) => generateArrayLiteral(xs)
        case T_NewPair(x1, x2) => generateNewPair(x1, x2)

    private def generate(t: T_Func): A_Func = ???

    private def generateDecl(v: T_Name, r: T_RValue): List[A_Instr] = ???

    private def generateAsgn(l: T_LValue, r: T_RValue): List[A_Instr] = ???

    private def generateRead(l: T_LValue): List[A_Instr] = ???

    private def generateFree(x: T_Expr): List[A_Instr] = ???

    private def generateReturn(x: T_Expr): List[A_Instr] = ???

    private def generateExit(x: T_Expr): List[A_Instr] = ???

    private def generatePrint(x: T_Expr): List[A_Instr] = ???

    private def generatePrintln(x: T_Expr): List[A_Instr] = ???

    private def generateIf(cond: T_Expr, body: List[T_Stmt], scopedBody: Set[T_Name], el: List[T_Stmt], scopedEl: Set[T_Name]): List[A_Instr] = ???

    private def generateWhile(cond: T_Expr, body: List[T_Stmt], scoped: Set[T_Name]): List[A_Instr] = ???

    private def generateCodeBlock(body: List[T_Stmt], scoped: Set[T_Name]): List[A_Instr] = ???

    private def generateSkip(): List[A_Instr] = ???

    private def generateMul(x: T_Expr, y: T_Expr): List[A_Instr] = ???

    private def generateDiv(x: T_Expr, y: T_Expr): List[A_Instr] = ???

    private def generateMod(x: T_Expr, y: T_Expr): List[A_Instr] = ???

    private def generateAdd(x: T_Expr, y: T_Expr): List[A_Instr] = 
        generate(x) ++ 
        (A_Push(A_Reg(intSize, A_RegName.RetReg)) :: 
        generate(y)) ++ 
        (A_Pop(A_Reg(intSize, A_RegName.R1)) ::
        A_Add(A_Reg(intSize, A_RegName.RetReg), A_Reg(intSize, A_RegName.R1), intSize) :: Nil)

    private def generateSub(x: T_Expr, y: T_Expr): List[A_Instr] = ???

    private def generateGreaterThan(x: T_Expr, y: T_Expr): List[A_Instr] = ???

    private def generateGreaterThanEq(x: T_Expr, y: T_Expr): List[A_Instr] = ???

    private def generateLessThan(x: T_Expr, y: T_Expr): List[A_Instr] = ???

    private def generateLessThanEq(x: T_Expr, y: T_Expr): List[A_Instr] = ???

    private def generateEq(x: T_Expr, y: T_Expr): List[A_Instr] = ???

    private def generateNotEq(x: T_Expr, y: T_Expr): List[A_Instr] = ???

    private def generateAnd(x: T_Expr, y: T_Expr): List[A_Instr] = ???

    private def generateOr(x: T_Expr, y: T_Expr): List[A_Instr] = ???

    private def generateNot(x: T_Expr): List[A_Instr] = ???

    private def generateNeg(x: T_Expr): List[A_Instr] = ???

    private def generateLen(x: T_Expr): List[A_Instr] = ???

    private def generateOrd(x: T_Expr): List[A_Instr] = ???

    private def generateChr(x: T_Expr): List[A_Instr] = ???

    private def generateIntLiteral(v: BigInt): List[A_Instr] = ???

    private def generateBoolLiteral(v: Boolean): List[A_Instr] = ???

    private def generateCharLiteral(v: Char): List[A_Instr] = ???

    private def generateStringLiteral(v: String): List[A_Instr] = ???

    private def generateIdent(v: T_Name): List[A_Instr] = ???

    private def generateArrayElem(v: T_Name, indices: List[T_Expr]): List[A_Instr] = ???

    private def generatePairNullLiteral(): List[A_Instr] = ???

    private def generatePairElem(index: PairIndex, v: T_LValue): List[A_Instr] = ???

    private def generateFuncCall(v: T_Name, args: List[T_Expr]): List[A_Instr] = ???

    private def generateArrayLiteral(xs: List[T_Expr]): List[A_Instr] = ???

    private def generateNewPair(x1: T_Expr, x2: T_Expr): List[A_Instr] = ???
}