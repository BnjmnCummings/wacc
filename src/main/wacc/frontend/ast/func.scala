package wacc.ast

/**
  * A program represented as an Abstract Syntax Tree.
  * @param funcs list of functions defined in the program.
  * @param body the body of the program as a list of statements.
  * @param pos position information (line, char) used for locating errors.
  */
case class Prog(funcs: List[Func], body: List[Stmt])(val pos: (Int, Int))

/**
  * A function AST node.
  * @param t the return type of the function.
  * @param name the unique name of the function.
  * @param args the list of parameters passed into the function
  * @param body the function body as a list of statements.
  * @param pos position information (line, char) used for locating errors.
  */
case class Func(t: Type, name: String, args: List[Param], body: List[Stmt])(val pos: (Int, Int))

/**
  * A Parameter AST node.
  * @param t the type of the parameter.
  * @param name parameter's name as [[Name]] object.
  * @param pos position information (line, char) used for locating errors.
  */
case class Param(t: Type, name: String)(val pos: (Int, Int))

/**
  * Companion object for default positioning and error labeling.
  */
object Prog extends ParserBridgePos2[List[Func], List[Stmt], Prog] {
    def apply(funcs: List[Func], body: List[Stmt]): Prog = Prog(funcs, body)((0, 0))
}
object Param extends ParserBridgePos2[Type, String, Param] {
    def apply(t: Type, v: String): Param = Param(t, v)((0, 0))
    override def labels = List("function parameters")
}
object Func extends ParserBridgePos4[Type, String, List[Param], List[Stmt], Func] {
    def apply(t: Type, v: String, args: List[Param], body: List[Stmt]): Func = Func(t, v, args, body)((0, 0))
    override def labels = List("function declaration")
}
