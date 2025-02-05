package wacc.q_ast

import wacc.ast.*
import parsley.generic


case class Q_Prog(funcs: List[Q_Func], body: List[Q_Stmt], scoped: Set[Q_Name])
case class Q_Func(t: Type, v: Q_Name, args: List[Q_Param], body: List[Q_Stmt], scoped: Set[Q_Name])
case class Q_Param(t: Type, v: Q_Name)

object Q_Prog extends generic.ParserBridge3[List[Q_Func], List[Q_Stmt], Set[Q_Name], Q_Prog]
object Q_Param extends generic.ParserBridge2[Type, Q_Name, Q_Param]
object Q_Func extends generic.ParserBridge5[Type, Q_Name, List[Q_Param], List[Q_Stmt], Set[Q_Name], Q_Func]
