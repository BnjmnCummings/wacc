package wacc.q_ast

import parsley.generic

case class Q_Prog(Q_funcs: List[Q_Func], Q_body: List[Q_Stmt])
case class Q_Func(Q_t: Q_Type, Q_v: String, Q_args: List[Q_Param], Q_body: List[Q_Stmt])
case class Q_Param(Q_t: Q_Type, Q_v: String)

object Q_Prog extends generic.ParserBridge2[List[Q_Func], List[Q_Stmt], Q_Prog]
object Q_Param extends generic.ParserBridge2[Q_Type, String, Q_Param]
object Q_Func extends generic.ParserBridge4[Q_Type, String, List[Q_Param], List[Q_Stmt], Q_Func]
