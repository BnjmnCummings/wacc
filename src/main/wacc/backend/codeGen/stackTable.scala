package wacc.codeGen

import wacc.assemblyIR.*
import wacc.q_ast.Name
import wacc.TypeInfo

import scala.collection.mutable.ListBuffer
import scala.collection.mutable

inline def NEW_FUNCTION_OFFSET = 16 /* Offset produced by the function call */

/**
  * Class to represent the stack tables of a function.
  *
  * @param paramTable the stack table for the function's parameters.
  * @param startOffset the starting offset of the stack tables.
  */
class StackTables(paramTable: Option[StackTable], startOffset: Int = 0) {
    val scopedTables = ListBuffer[StackTable]()
    var endOffset = startOffset

    /**
     * Gets the sum of the sizes of each stack table.
     * @return the size of the stack tables.
     */
    def scopeSize = 
        scopedTables.map {_.size}.sum 

    /**
     * Gets the size of the parameters stack table.
     * @return the size of the parameters stack table.
     */
    def paramsSize = paramTable match
        case Some(t) => t.size
        case None    => 0

    /**
     * Adds a scope to the stack tables.
     * @param newScope the set of variables in the scope.
     * @param typeInfo the type information of the variables.
     */
    def addScope(newScope: Set[Name], typeInfo: TypeInfo): Unit = 
        val newTable = StackTable(endOffset)
        newTable.addScope(newScope, typeInfo)
        scopedTables += newTable
        endOffset += newTable.size

    /**
     * Gets the offset of a variable in the stack tables.
     * @param v the variable to get the offset of.
     * @return the offset of the variable.
     */
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

    /**
     * Sets the value of a variable in the stack tables.
     * @param v the variable to set the value of.
     * @return the list of instructions to set the value of the variable.
     */
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

    /**
     * Stores the arguments of a function in the stack tables.
     * @param args the arguments to store.
     * @return the list of instructions to store the arguments.
     */
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

/**
  * Class to represent a stack table, a mapping from variable names to their offsets on the stack.
  * @param basePtrOffset the offset from the base pointer.
  */
class StackTable(val basePtrOffset: Int) {
    val table = mutable.Map[Name, Int]()
    var size = 0

    /**
      * Checks if the table contains a variable.
      * @param v the variable to check for.
      * @return true if the table contains the variable, false otherwise.
      */
    def contains(v: Name): Boolean = 
        table.contains(v)

    /**
     * Adds a scope to the stack table.
     * @param scoped the set of variables in the scope.
     * @param typeInfo the type information of the variables.
     */
    def addScope(scoped: Set[Name], typeInfo: TypeInfo): Unit  =
        scoped.foreach { v => 
            size += intSizeOf(typeInfo.varTys(v))
            table += (v -> size)
        }

    /**
     * Gets the offset of a variable in the stack table by the apply method.
     * @param v the variable to get the offset of.
     * @return the offset of the variable.
     */
    def apply(v: Name): Int = 
        if (table.contains(v)) 
            table(v) + basePtrOffset
        else 
            throw RuntimeException("Variable " + v + " not found in stack tablee")
}
