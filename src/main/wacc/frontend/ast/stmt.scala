package wacc.ast

/**
  * AST nodes for Statements.
  */
sealed trait Stmt

/**
  * Declarations and Assignments.
  * @param v Name of the variable for a declaration.
  * @param r Right hand side of the assignment.
  * @param l Left hand side of the assignment.
  * @param pos position information (line, char) used for locating errors.
  */
case class Decl(t: Type, v: Ident, r: RValue)(val pos: (Int, Int)) extends Stmt
case class Asgn(l: LValue, r: RValue)(val pos: (Int, Int)) extends Stmt

/**
  * Standard library functions.
  * @param x Expression to be passed in.
  * @param pos position information (line, char) used for locating errors.
  */
case class Free(x: Expr)(val pos: (Int, Int)) extends Stmt
case class Return(x: Expr)(val pos: (Int, Int)) extends Stmt
case class Exit(x: Expr)(val pos: (Int, Int)) extends Stmt
case class Print(x: Expr)(val pos: (Int, Int)) extends Stmt
case class Println(x: Expr)(val pos: (Int, Int)) extends Stmt

/**
  * Read function call, only takes in an l value.
  * @param l the value to be read into (ident/pairElem/arrayElem).
  * @param pos position information (line, char) used for locating errors.
  */
case class Read(l: LValue)(val pos: (Int, Int)) extends Stmt

/**
  * @param cond Condition of the if statement.
  * @param body Body of the if statement.
  * @param el Else body of the if statement.
  * @param pos position information (line, char) used for locating errors.
  */
case class If(cond: Expr, body: List[Stmt], el: List[Stmt])(val pos: (Int, Int)) extends Stmt
case class While(cond: Expr, body: List[Stmt])(val pos: (Int, Int)) extends Stmt
case class CodeBlock(body: List[Stmt])(val pos: (Int, Int)) extends Stmt

/* Skip statement. */
case class Skip(val pos: (Int, Int)) extends Stmt

/**
  * Companion object for default positioning and error labeling.
  */
object Decl extends ParserBridgePos3[Type, Ident, RValue, Decl] {
    def apply(t: Type, v: Ident, r: RValue): Decl = Decl(t, v, r)((0, 0))
    override def labels = List("declaration")
}
object Asgn extends ParserBridgePos2[LValue, RValue, Asgn] {
    def apply(l: LValue, r: RValue): Asgn = Asgn(l, r)((0,0))
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
object Skip extends ParserBridgePos0[Skip] {
    def instance(): Skip = Skip((0,0))
    override def labels = List("skip statement")
}
