package wacc.codeGen

import scala.collection.mutable.ListBuffer
import wacc.q_ast.Name
import wacc.assemblyIR.*
import scala.collection.mutable
import wacc.TypeInfo

// Offset produced by the function call
def NEW_FUNCTION_OFFSET = 16

class StackTables(paramTable: Option[StackTable], startOffset: Int = 0) {
    val scopedTables = ListBuffer[StackTable]()

    var endOffset = startOffset

    def addScope(newScope: Set[Name], typeInfo: TypeInfo): Unit = {
        val newTable = StackTable(endOffset)

        newTable.addScope(newScope, typeInfo)

        scopedTables += newTable
        endOffset += newTable.size
    }

    def scopeSize = scopedTables.foldLeft(0)(_ + _.size)

    def paramsSize = paramTable match {
        case Some(t) => t.size
        case None => 0
    }

    def get(v: Name)(using codeGenCtx: CodeGenCtx): List[A_Instr] = {
        scopedTables.find(_.contains(v)) match {
            case Some(t) => 
                return List(A_MovTo(A_Reg(A_RegName.RetReg), A_RegDeref(A_MemOffset(A_Reg(A_RegName.BasePtr), A_OffsetImm(-t(v)))), sizeOf(codeGenCtx.typeInfo.varTys(v))))
            case None => paramTable match
                case Some(t) =>
                     if(t.contains(v)) {
                        return List(
                        A_MovTo(A_Reg(A_RegName.RetReg), A_RegDeref(A_MemOffset(A_Reg(A_RegName.BasePtr), A_OffsetImm(NEW_FUNCTION_OFFSET + t.size -t(v)))), sizeOf(codeGenCtx.typeInfo.varTys(v)))
                        ) 
                    } else { throw new RuntimeException("Variable " + v + " not found in stack tables") }
                case None => throw new RuntimeException("Variable " + v + " not found in stack table")
        }
    }

    def set(v: Name)(using codeGenCtx: CodeGenCtx): List[A_Instr] = {
        scopedTables.find(_.contains(v)) match {
            case Some(t) => 
                return List(A_MovFrom(A_RegDeref(A_MemOffset(A_Reg(A_RegName.BasePtr), A_OffsetImm(-t(v)))), A_Reg(A_RegName.RetReg), sizeOf(codeGenCtx.typeInfo.varTys(v))))
            case None => paramTable match
                case Some(t) => 
                    if(t.contains(v)) { 
                        return List(
                        A_MovFrom(A_RegDeref(A_MemOffset(A_Reg(A_RegName.BasePtr), A_OffsetImm(NEW_FUNCTION_OFFSET + t.size -t(v)))), A_Reg(A_RegName.RetReg), sizeOf(codeGenCtx.typeInfo.varTys(v)))
                        ) 
                    } else { throw new RuntimeException("Variable " + v + " not found in stack tables") }
                case None => throw new RuntimeException("Variable " + v + " not found in stack table")
        }
    }

    def argStoreInstrs(args: List[Name])(using codeGenCtx: CodeGenCtx): List[A_Instr] = {
        paramTable match
            case Some(t) => args.map( {v =>
                A_MovFrom(A_RegDeref(A_MemOffset(A_Reg(A_RegName.StackPtr), A_OffsetImm(paramsSize - t.table(v)))), A_Reg(A_RegName.RetReg), sizeOf(codeGenCtx.typeInfo.varTys(v)))
                } )
            case None => throw new RuntimeException("No parameter table found when calling on a function's stack table")
    }
}


class StackTable(val basePtrOffset: Int) {
    val table = mutable.Map[Name, Int]()

    var size = 0

    def apply(v: Name): Int = {
        if (table.contains(v)) {
            return table(v) + basePtrOffset
        }
        throw new RuntimeException("Variable " + v + " not found in stack tablee")
    }

    def contains(v: Name) = table.contains(v)

    def addScope(scoped: Set[Name], typeInfo: TypeInfo): Unit = {
        scoped.foreach(v => {
            size += intSizeOf(typeInfo.varTys(v))
            table += (v -> size)
        })
    }
}