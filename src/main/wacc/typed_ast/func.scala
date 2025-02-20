package wacc.t_ast

import wacc.ast.*

case class Q_Prog(funcs: List[Q_Func], body: List[Q_Stmt], scoped: Set[Q_Name], pos: (Int, Int) = (0, 0))
case class Q_Func(t: Type, v: Q_Name, args: List[Q_Param], body: List[Q_Stmt], scoped: Set[Q_Name], pos: (Int, Int) = (0, 0))
case class Q_Param(t: Type, v: Q_Name, pos: (Int, Int) = (0, 0))
