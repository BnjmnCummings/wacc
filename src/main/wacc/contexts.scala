package wacc

import collection.mutable.ListBuffer

import q_ast.Q_Name

trait ErrContext {
    def fname: Option[String]
    def pos: (Int, Int)
}


class RenamerContext(val errors: ListBuffer[Err] = ListBuffer[Err](), fnameIn: Option[String] = None, var posVal: (Int, Int) = (0, 0)) extends ErrContext {
    def fname: Option[String] = fnameIn

    def pos: (Int, Int) = posVal

    def setPos(newPos: (Int, Int)) = {
        posVal = newPos
    }
    
    def getErrors: List[Err] = errors.toList
}

class TypeCheckerCtx(tyInfo: TypeInfo, errs: ListBuffer[Err], fnameIn: Option[String] = None, posIn: (Int, Int) = (0,0)) extends ErrContext{
    def errors: List[Err] = errs.toList

    def fname: Option[String] = fnameIn

    def pos: (Int, Int) = posIn

    // This will get the type of variables
    def typeOf(id: Q_Name): KnownType = tyInfo.varTys(id)

    def error(err: Err) = {
        errs += err
        None
    }
}