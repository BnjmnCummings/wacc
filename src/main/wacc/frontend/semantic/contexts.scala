package wacc

import wacc.q_ast.Name

import scala.collection.mutable.ListBuffer
import scala.collection.mutable

/**
  * A context for keeping track of error information.
  */
trait ErrContext {
    def fname: Option[String]
    def pos: (Int, Int)
}

/**
  * A context for keeping track of scope error information.
  * @param errors list of errors collected.
  * @param fnameIn the filename of the .wacc program.
  * @param posVal the position in .wacc program we reached.
  */
class RenamerContext(val errors: ListBuffer[Err] = ListBuffer[Err](), fnameIn: Option[String] = None, var posVal: (Int, Int) = (1, 1)) extends ErrContext {
    def fname: Option[String] = 
        fnameIn

    def pos: (Int, Int) = 
        posVal

    def setPos(newPos: (Int, Int)) = 
        posVal = newPos
    
    def getErrors: List[Err] = 
        errors.toList
}

/**
  * A context for keeping track of type error information.
  * @param tyInfo type information about the functions and variables.
  * @param errors list of errors collected.
  * @param fnameInthe filename of the .wacc program.
  * @param posValthe position in .wacc program we reached.
  */
class TypeCheckerCtx(tyInfo: TypeInfo, errors: ListBuffer[Err], fnameIn: Option[String] = None, var posVal: (Int, Int) = (1, 1)) extends ErrContext{
    def getErrors: List[Err] = 
        errors.toList

    def fname: Option[String] = 
        fnameIn

    def pos: (Int, Int) = 
        posVal

    def setPos(newPos: (Int, Int)): Unit = 
        posVal = newPos
    
    def typeOf(id: Name): KnownType = 
        tyInfo.varTys(id)
    
    def typeOfFunc(id: Name): (KnownType, List[Name]) = 
        tyInfo.funcTys(id)

    def error(err: Err) =
        errors += err
        None
}