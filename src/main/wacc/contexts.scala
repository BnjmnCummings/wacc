package wacc

import collection.mutable.ListBuffer

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
