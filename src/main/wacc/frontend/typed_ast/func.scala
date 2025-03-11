package wacc.t_ast

import wacc.ast.Type
import wacc.q_ast.Name

case class T_Prog(funcs: List[T_Func], body: List[T_Stmt], scoped: Set[Name])
case class T_Func(t: Type, name: Name, args: List[T_Param], body: List[T_Stmt], scoped: Set[Name])
case class T_Param(t: Type, name: Name)
