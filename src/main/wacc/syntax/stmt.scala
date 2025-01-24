package wacc.syntax

sealed trait Stmt

case class Decl(t: Type, v: Ident, r: RValue) extends Stmt
case class Asgn(l: LValue, r: RValue) extends Stmt
case class Read(l: LValue) extends Stmt
case class Free(x: Expr) extends Stmt
case class Return(x: Expr) extends Stmt
case class Exit(x: Expr) extends Stmt
case class Print(x: Expr) extends Stmt
case class Println(x: Expr) extends Stmt
case class If(cond: Expr, body: List[Stmt], el: List[Stmt]) extends Stmt
case class While(cond: Expr, body: List[Stmt]) extends Stmt
case class CodeBlock(body: List[Stmt]) extends Stmt

object Skip extends Stmt