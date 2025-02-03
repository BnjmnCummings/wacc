package wacc.q_ast

import wacc.ast.*
import parsley.generic


case class Q_Prog(funcs: List[Q_Func], body: List[Q_Stmt])
case class Q_Func(t: Type, v: String, args: List[Q_Param], body: List[Q_Stmt])
case class Q_Param(t: Type, v: String)

object Q_Prog extends generic.ParserBridge2[List[Q_Func], List[Q_Stmt], Q_Prog]
object Q_Param extends generic.ParserBridge2[Type, String, Q_Param]
object Q_Func extends generic.ParserBridge4[Type, String, List[Q_Param], List[Q_Stmt], Q_Func]
