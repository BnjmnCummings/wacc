package wacc.syntax

case class Prog(funcs: List[Func], body: List[Stmt])(val pos: (Int, Int))
case class Func(t: Type, v: String, args: List[Param], body: List[Stmt])(val pos: (Int, Int))
case class Param(t: Type, v: String)(val pos: (Int, Int))

object Prog extends ParserBridgePos2[List[Func], List[Stmt], Prog]
object Param extends ParserBridgePos2[Type, String, Param]
object Func extends ParserBridgePos4[Type, String, List[Param], List[Stmt], Func]
