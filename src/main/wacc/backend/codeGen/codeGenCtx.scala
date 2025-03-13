package wacc.codeGen

import wacc.assemblyIR.*
import wacc.TypeInfo

import scala.collection.mutable.Set as MutableSet

/**
  * Context class for code generation. Collects information about the program being compiled including:
     default functions, stored strings and type information.
  * @param typeInfo
  * @param stackTables
  */
class CodeGenCtx(val typeInfo: TypeInfo, val stackTables: TableCtx) {
    private val defaultFuncs: MutableSet[A_Func] = MutableSet()
    private val storedStrings: MutableSet[A_StoredStr] = MutableSet()
    private var strLabelCount: Int = 0
    private var instrLabelCount: Int = 0

    /**
     * Getter for the stored strings list.
     * @return the list of stored strings.
     */
    def getStoredStrings: List[A_StoredStr] = 
        storedStrings.toList

    /**
      * Adds a string to the list of strings in the context.
      * @param label the unique label of the string.
      * @param str the string to add.
      */
    def addStoredStr(label: A_DataLabel, str: String): Unit = 
        storedStrings.add(A_StoredStr(label, str))
    
    /**
      * Getter for the default function list.
      * @return the list of default functions.
      */
    def getDefaultFuncs: List[A_Func] = 
        defaultFuncs.toList

    /**
      * Adds a default function and it's dependancies to the context.
      * @param label the label of the default function to add.
      */
    def addDefaultFunc(label: A_DefaultLabel): Unit = 
        defaultFuncs.add(deFuncMap(label))
        deFuncDependancyMap(label).foreach { addDefaultFunc }
        deFuncStringDependancyMap(label).foreach { addStoredStr }

    /**
      * Generates a new unique instruction label.
      * Incraments the instruction label count.
      * @return the new label.
      */
    def genNextInstrLabel(): A_InstrLabel = 
        val label = A_InstrLabel(s".L$instrLabelCount")
        instrLabelCount += 1
        label

    /**
      * Generates a label for a stored string and adds it to the list of stored strings
      * Checks for if the string has already been stored and returns the label if it has.
      * @param str the string to store
      * @return the label of the string.
      */
    def storeString(str: String): A_DataLabel =
        storedStrings.find {_.str == str } match
            case None => 
                val label = genNextStrLabel
                storedStrings.add(A_StoredStr(label, str))
                label
            case Some(value) => value.lbl
    
    /**
      * Helper function to generate a new unique label for a stored string.
      * Increments the string label count.
      * @return the new label.
      */
    private def genNextStrLabel: A_DataLabel = 
        val num = strLabelCount
        strLabelCount += 1
        A_DataLabel(s".S.str${num}")
}
