package wacc.q_ast

import wacc.ast.*

/**
  * A program represented as a Qualified AST.
  * @param funcs list of functions defined in the program.
  * @param body the body of the program as a list of statements.
  * @param scoped the variables available in the top-most scope of the program.
  * @param pos position information (line, char) used for locating errors.
  */
case class Q_Prog(funcs: List[Q_Func], body: List[Q_Stmt], scoped: Set[Name], pos: (Int, Int) = (0, 0))

/**
  * A typed function.
  * @param t the return type of the function.
  * @param name the unique name of the function.
  * @param args the list of parameters passed into the function
  * @param body the function body as a list of statements.
  * @param scoped the variables available in the top-most scope of the function.
  * @param pos position information (line, char) used for locating errors.
  */
case class Q_Func(t: Type, name: Name, args: List[Q_Param], body: List[Q_Stmt], scoped: Set[Name], pos: (Int, Int) = (0, 0))

/**
  * A typed Parameter.
  * @param t the type of the parameter.
  * @param name parameter's name as [[Name]] object.
  * @param pos position information (line, char) used for locating errors.
  */
case class Q_Param(t: Type, name: Name, pos: (Int, Int) = (0, 0))
