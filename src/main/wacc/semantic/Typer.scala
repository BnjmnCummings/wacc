package wacc.semantic

import wacc.syntax.* 

case class TypedProg(funcs: List[TypedFunc], body: List[TypedStmt])
case class TypedFunc(t: Type, id: Ident, args: List[TypedParam], body: List[TypedStmt])
case class TypedParam(t: Type, id: Ident)