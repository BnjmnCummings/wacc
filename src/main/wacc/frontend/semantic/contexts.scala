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
    