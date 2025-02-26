package wacc.q_ast

sealed trait Q_Stmt
case class Q_Decl(v: Name, r: Q_RValue, pos: (Int, Int) = (0, 0)) extends Q_Stmt
case class Q_Asgn(l: Q_LValue, r: Q_RValue, pos: (Int, Int) = (0, 0)) extends Q_Stmt
case class Q_Read(l: Q_LValue, pos: (Int, Int) = (0, 0)) extends Q_Stmt
case class Q_Free(x: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_Stmt
case class Q_Return(x: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_Stmt
case class Q_Exit(x: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_Stmt
case class Q_Print(x: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_Stmt
case class Q_Println(x: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_Stmt
case class Q_If(cond: Q_Expr, body: List[Q_Stmt], scopedBody: Set[Name], el: List[Q_Stmt], scopedEl: Set[Name], pos: (Int, Int) = (0, 0)) extends Q_Stmt
case class Q_While(cond: Q_Expr, body: List[Q_Stmt], scoped: Set[Name], pos: (Int, Int) = (0, 0)) extends Q_Stmt
case class Q_CodeBlock(body: List[Q_Stmt], scoped: Set[Name], pos: (Int, Int) = (0, 0)) extends Q_Stmt
case class Q_Skip(pos: (Int, Int) = (0, 0)) extends Q_Stmt
