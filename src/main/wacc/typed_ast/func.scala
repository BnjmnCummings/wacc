package wacc.t_ast

import wacc.ast.*

case class T_Prog(funcs: List[T_Func], body: List[T_Stmt], scoped: Set[T_Name], pos: (Int, Int) = (0, 0))
case class T_Func(t: Type, v: T_Name, args: List[T_Param], body: List[T_Stmt], scoped: Set[T_Name], pos: (Int, Int) = (0, 0))
case class T_Param(t: Type, v: T_Name, pos: (Int, Int) = (0, 0))
