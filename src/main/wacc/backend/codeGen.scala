package wacc.codeGen

import wacc.t_ast.*
import wacc.assemblyIR.*
import wacc.TypeInfo

import scala.collection.mutable.ListBuffer

class CodeGen(t_tree: T_Prog, typeInfo: TypeInfo) {
    private val storedStrings: ListBuffer[A_StoredStr] = ListBuffer()

    def generate(): A_Prog = ???
}