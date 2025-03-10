package wacc.codeGen

import wacc.assemblyIR.*
import wacc.q_ast.Name
import wacc.TypeInfo

import scala.collection.mutable.ListBuffer
import scala.collection.mutable

inline def NEW_FUNCTION_OFFSET = 16 /* Offset produced by the function call */

class StackTables(paramTable: Option[StackTable], startOffset: Int = 0) {
    val scopedTables = ListBuffer[StackTable]()
    var endOffset = startOffset

    def scopeSize = 
        scopedTables.foldLeft(0)(_ + _.size)

    def paramsSize = paramTable match
        case Some(t) => t.size
        case None    => 0

    def addScope(newScope: Set[Name], typeInfo: TypeInfo): Unit = 
        val newTable = StackTable(endOffset)
        newTable.addScope(newScope, typeInfo)
        scopedTables += newTable
        endOffset += newTable.size

    def get(v: Name)(using codeGenCtx: CodeGenCtx): List[A_Instr] =
        scopedTables.find(_.contains(v)) match 
            case Some(t) => 
                List(A_Mov(
                    A_Reg(A_RegName.RetReg), 
                    A_RegDeref(A_MemOffset(A_Reg(A_RegName.BasePtr), A_OffsetImm(-t(v)))), 
                    sizeOf(codeGenCtx.typeInfo.varTys(v))))

            case None => paramTable match
                case Some(t) =>
                    if(t.contains(v))
                        List(A_Mov(
                            A_Reg(A_RegName.RetReg), 
                            A_RegDeref(A_MemOffset(A_Reg(A_RegName.BasePtr), A_OffsetImm(NEW_FUNCTION_OFFSET + t.size -t(v)))), 
                            sizeOf(codeGenCtx.typeInfo.varTys(v)))) 
                    else 
                        throw RuntimeException("Variable " + v + " not found in stack tables")

                case None => 
                    throw RuntimeException("Variable " + v + " not found in stack table")

    def set(v: Name)(using codeGenCtx: CodeGenCtx): List[A_Instr] = 
        scopedTables.find(_.contains(v)) match
            case Some(t) => 
                List(A_Mov(
                    A_RegDeref(A_MemOffset(A_Reg(A_RegName.BasePtr), A_OffsetImm(-t(v)))), 
                    A_Reg(A_RegName.RetReg), 
                    sizeOf(codeGenCtx.typeInfo.varTys(v))))

            case None => paramTable match
                case Some(t) => 
                    if(t.contains(v))
                        List(A_Mov(
                            A_RegDeref(A_MemOffset(A_Reg(A_RegName.BasePtr), A_OffsetImm(NEW_FUNCTION_OFFSET + t.size -t(v)))), 
                            A_Reg(A_RegName.RetReg), 
                            sizeOf(codeGenCtx.typeInfo.varTys(v)))) 
                    else 
                        throw RuntimeException("Variable " + v + " not found in stack tables")

                case None => 
                    throw RuntimeException("Variable " + v + " not found in stack table")

    def argStoreInstrs(args: List[Name])(using codeGenCtx: CodeGenCtx): List[A_Instr] = 
        paramTable match
            case Some(t) => 
                args.map { v =>
                    A_Mov(
                        A_RegDeref(A_MemOffset(A_Reg(A_RegName.StackPtr), 
                        A_OffsetImm(paramsSize - t.table(v)))), 
                        A_Reg(A_RegName.RetReg), 
                        sizeOf(codeGenCtx.typeInfo.varTys(v)))
                }
            case None => 
                throw RuntimeException("No parameter table found when calling on a function's stack table")
}

class StackTable(val basePtrOffset: Int) {
    val table = mutable.Map[Name, Int]()
    var size = 0

    def contains(v: Name): Boolean = 
        table.contains(v)

    def addScope(scoped: Set[Name], typeInfo: TypeInfo): Unit  =
        scoped.foreach { v => 
            size += intSizeOf(typeInfo.varTys(v))
            table += (v -> size)
        }

    def apply(v: Name): Int = 
        if (table.contains(v)) 
            table(v) + basePtrOffset
        else 
            throw RuntimeException("Variable " + v + " not found in stack tablee")
}
