package wacc.syntax

import parsley.generic

sealed trait Stmt

case class Decl(t: Type, v: String, r: RValue) extends Stmt
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

object Decl extends generic.ParserBridge3[Type, String, RValue, Decl]
object Asgn extends generic.ParserBridge2[LValue, RValue, Asgn]
object Read extends generic.ParserBridge1[LValue, Read]
object Free extends generic.ParserBridge1[Expr, Free]
object Return extends generic.ParserBridge1[Expr, Return]
object Exit extends generic.ParserBridge1[Expr, Exit]
object Print extends generic.ParserBridge1[Expr, Print]
object Println extends generic.ParserBridge1[Expr, Println]
object If extends generic.ParserBridge3[Expr, List[Stmt], List[Stmt], If]
object While extends generic.ParserBridge2[Expr, List[Stmt], While]
object CodeBlock extends generic.ParserBridge1[List[Stmt], CodeBlock]

object Skip extends Stmt
