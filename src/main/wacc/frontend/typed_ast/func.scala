package wacc.t_ast

import wacc.ast.Type
import wacc.q_ast.Name

/**
  * A program represented as a Typed AST.
  * @param funcs list of functions defined in the program.
  * @param body the body of the program as a list of statements.
  * @param scoped the variables available in the top-most scope of the program.
  */
case class T_Prog(funcs: List[T_Func], body: List[T_Stmt], scoped: Set[Name])

/**
  * A typed function.
  * @param t the return type of the function.
  * @param name the unique name of the function.
  * @param args the list of parameters passed into the function
  * @param body the function body as a list of statements.
  * @param scoped the variables available in the top-most scope of the function.
  */
case class T_Func(t: Type, name: Name, args: List[T_Param], body: List[T_Stmt], scoped: Set[Name])

/**
  * A typed Parameter.
  * @param t the type of the parameter.
  * @param name parameter's name as [[Name]] object.
  */
case class T_Param(t: Type, name: Name)
