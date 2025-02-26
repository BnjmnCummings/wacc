package wacc

import collection.mutable.ListBuffer
import collection.mutable

import wacc.assemblyIR.*
import q_ast.Q_Name

trait ErrContext {
    def fname: Option[String]
    def pos: (Int, Int)
}


class RenamerContext(val errors: ListBuffer[Err] = ListBuffer[Err](), fnameIn: Option[String] = None, var posVal: (Int, Int) = (1, 1)) extends ErrContext {
    def fname: Option[String] = fnameIn

    def pos: (Int, Int) = posVal

    def setPos(newPos: (Int, Int)) = {
        posVal = newPos
    }
    
    def getErrors: List[Err] = errors.toList
}

class TypeCheckerCtx(tyInfo: TypeInfo, errs: ListBuffer[Err], fnameIn: Option[String] = None, var posVal: (Int, Int) = (1, 1)) extends ErrContext{
    def errors: List[Err] = errs.toList

    def fname: Option[String] = fnameIn

    def pos: (Int, Int) = posVal

    def setPos(newPos: (Int, Int)) = {
        posVal = newPos
    }

    // This will get the type of variables
    def typeOf(id: Q_Name): KnownType = tyInfo.varTys(id)
    def typeOfFunc(id: Q_Name): (KnownType, List[Q_Name]) = tyInfo.funcTys(id)

    def error(err: Err) = {
        errs += err
        None
    }
}

class CodeGenCtx {
    private val storedStrings: mutable.Set[A_StoredStr] = mutable.Set()

    private val defaultFuncs: mutable.Set[A_Func] = mutable.Set()

    def addDefaultFunc(f: A_Func) = defaultFuncs.add(f)

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
    // TODO @Jack : Create a function that maps a KnownType to a size - this is useful for things like read (char/int)

    private var instrLabelCount: Int = 0

    def genNextInstrLabel(): A_InstrLabel = {
        val lbl = A_InstrLabel(s".L$instrLabelCount")
        instrLabelCount += 1
        lbl
    }
}
    