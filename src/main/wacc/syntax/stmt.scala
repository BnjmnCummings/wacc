package wacc.syntax

sealed trait Stmt

case class Decl(t: Type, v: String, r: RValue)(val pos: (Int, Int)) extends Stmt
case class Asgn(l: LValue, r: RValue)(val pos: (Int, Int)) extends Stmt
case class Read(l: LValue)(val pos: (Int, Int)) extends Stmt
case class Free(x: Expr)(val pos: (Int, Int)) extends Stmt
case class Return(x: Expr)(val pos: (Int, Int)) extends Stmt
case class Exit(x: Expr)(val pos: (Int, Int)) extends Stmt
case class Print(x: Expr)(val pos: (Int, Int)) extends Stmt
case class Println(x: Expr)(val pos: (Int, Int)) extends Stmt
case class If(cond: Expr, body: List[Stmt], el: List[Stmt])(val pos: (Int, Int)) extends Stmt
case class While(cond: Expr, body: List[Stmt])(val pos: (Int, Int)) extends Stmt
case class CodeBlock(body: List[Stmt])(val pos: (Int, Int)) extends Stmt

object Decl extends ParserBridgePos3[Type, String, RValue, Decl]
object Asgn extends ParserBridgePos2[LValue, RValue, Asgn]
object Read extends ParserBridgePos1[LValue, Read]
object Free extends ParserBridgePos1[Expr, Free]
object Return extends ParserBridgePos1[Expr, Return]
object Exit extends ParserBridgePos1[Expr, Exit]
object Print extends ParserBridgePos1[Expr, Print]
object Println extends ParserBridgePos1[Expr, Println]
object If extends ParserBridgePos3[Expr, List[Stmt], List[Stmt], If]
object While extends ParserBridgePos2[Expr, List[Stmt], While]
object CodeBlock extends ParserBridgePos1[List[Stmt], CodeBlock]

object Skip extends Stmt
