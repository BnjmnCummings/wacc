package wacc.codeGen

import wacc.assemblyIR.*
import wacc.TypeInfo

import scala.collection.mutable

class CodeGenCtx(val typeInfo: TypeInfo) {
    private val storedStrings: mutable.Set[A_StoredStr] = mutable.Set()

    private val defaultFuncs: mutable.Set[A_Func] = mutable.Set()

    def addDefaultFunc(f: A_Func) = defaultFuncs.add(f)

    def defaultFuncsList: List[A_Func] = defaultFuncs.toList

    private var strLabelCount = 0

    def genNextStrLabel: A_DataLabel = {
        val num = strLabelCount
        strLabelCount += 1
        A_DataLabel(s".S.str${num}")
    }

    def genStoredStr(str: String): A_DataLabel = {
        if (storedStrings.exists(_.str == str)) {
            storedStrings.find(_.str == str).get.lbl
        } else {
            val lbl = genNextStrLabel
            storedStrings.add(A_StoredStr(lbl, str))
            lbl
        }
    }

    def storedStringsList: List[A_StoredStr] = storedStrings.toList
    // TODO @Jack : Create a function that maps a KnownType to a size - this is useful for things like read (char/int)

    private var instrLabelCount: Int = 0

    def genNextInstrLabel(): A_InstrLabel = {
        val lbl = A_InstrLabel(s".L$instrLabelCount")
        instrLabelCount += 1
        lbl
    }
}
