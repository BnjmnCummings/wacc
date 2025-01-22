case class Prog(funcs: List[Func], body: List[Stmt])

case class Func(t: Type, v: Ident, args: List[Param], body: List[Stmt])

case class Param(t: Type, v: Ident)