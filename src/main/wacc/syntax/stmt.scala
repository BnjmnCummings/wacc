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
case class Skip(val pos: (Int, Int)) extends Stmt

object Decl extends ParserBridgePos3[Type, String, RValue, Decl] {
    def apply(t: Type, v: String, r: RValue): Decl = Decl(t, v, r)((0, 0))
    override def labels = List("decleration")
}
object Asgn extends ParserBridgePos2[LValue, RValue, Asgn] {
    def apply(l: LValue, r: RValue): Asgn = Asgn(l, r)((0, 0))
    override def labels = List("assignment")
}
object Read extends ParserBridgePos1[LValue, Read] {
    def apply(l: LValue): Read = Read(l)((0, 0))
    override def labels = List("read statement")
}
object Free extends ParserBridgePos1[Expr, Free] {
    def apply(x: Expr): Free = Free(x)((0, 0))
    override def labels = List("free statement")
}
object Return extends ParserBridgePos1[Expr, Return] {
    def apply(x: Expr): Return = Return(x)((0, 0))
    override def labels = List("return statement")
}
object Exit extends ParserBridgePos1[Expr, Exit] {
    def apply(x: Expr): Exit = Exit(x)((0, 0))
    override def labels = List("exit statement")
}
object Print extends ParserBridgePos1[Expr, Print] {
    def apply(x: Expr): Print = Print(x)((0, 0))
    override def labels = List("print statement")
}
object Println extends ParserBridgePos1[Expr, Println] {
    def apply(x: Expr): Println = Println(x)((0, 0))
    override def labels = List("println statement")
}
object If extends ParserBridgePos3[Expr, List[Stmt], List[Stmt], If] {
    def apply(cond: Expr, body: List[Stmt], el: List[Stmt]): If = If(cond, body, el)((0, 0))
    override def labels = List("if-else statement")
}
object While extends ParserBridgePos2[Expr, List[Stmt], While] {
    def apply(cond: Expr, body: List[Stmt]): While = While(cond, body)((0, 0))
    override def labels = List("while loop")
}
object CodeBlock extends ParserBridgePos1[List[Stmt], CodeBlock] {
    def apply(body: List[Stmt]): CodeBlock = CodeBlock(body)((0, 0))
    override def labels = List("begin-end block")
}

object Skip extends Stmt, ParserBridgePos0[Skip] {
    override def labels = List("skip statement")
}
