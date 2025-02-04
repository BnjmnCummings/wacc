package wacc.syntax

case class Prog(funcs: List[Func], body: List[Stmt])(val pos: (Int, Int))
case class Func(t: Type, v: String, args: List[Param], body: List[Stmt])(val pos: (Int, Int))
case class Param(t: Type, v: String)(val pos: (Int, Int))

object Prog extends ParserBridgePos2[List[Func], List[Stmt], Prog] {
    def apply(funcs: List[Func], body: List[Stmt]): Prog = Prog(funcs, body)((0, 0))
}
object Param extends ParserBridgePos2[Type, String, Param] {
    def apply(t: Type, v: String): Param = Param(t, v)((0, 0))
}
object Func extends ParserBridgePos4[Type, String, List[Param], List[Stmt], Func] {
    def apply(t: Type, v: String, args: List[Param], body: List[Stmt]): Func = Func(t, v, args, body)((0, 0))
}
