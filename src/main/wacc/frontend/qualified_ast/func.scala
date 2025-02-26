package wacc.q_ast

import wacc.ast.*

case class Q_Prog(funcs: List[Q_Func], body: List[Q_Stmt], scoped: Set[Name], pos: (Int, Int) = (0, 0))
case class Q_Func(t: Type, v: Name, args: List[Q_Param], body: List[Q_Stmt], scoped: Set[Name], pos: (Int, Int) = (0, 0))
case class Q_Param(t: Type, v: Name, pos: (Int, Int) = (0, 0))
