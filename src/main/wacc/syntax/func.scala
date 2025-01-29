package wacc.syntax

import parsley.generic


case class Prog(funcs: List[Func], body: List[Stmt])
case class Func(t: Type, v: String, args: List[Param], body: List[Stmt])
case class Param(t: Type, v: String)

object Prog extends generic.ParserBridge2[List[Func], List[Stmt], Prog]
object Param extends generic.ParserBridge2[Type, String, Param]
object Func extends generic.ParserBridge4[Type, String, List[Param], List[Stmt], Func]
