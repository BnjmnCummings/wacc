package wacc.t_ast

import wacc.SemType

sealed trait T_Stmt // TODO: Add types for read, free, return, etc.
case class T_Decl(v: T_Name, r: T_RValue) extends T_Stmt
case class T_Asgn(l: T_LValue, r: T_RValue) extends T_Stmt
case class T_Read(l: T_LValue) extends T_Stmt // int, char
case class T_Free(x: T_Expr) extends T_Stmt // t[], pair(t, o)
case class T_Return(x: T_Expr) extends T_Stmt // t
case class T_Exit(x: T_Expr) extends T_Stmt
case class T_Print(x: T_Expr) extends T_Stmt // t
case class T_Println(x: T_Expr) extends T_Stmt // t
case class T_If(cond: T_Expr, body: List[T_Stmt], scopedBody: Set[T_Name], el: List[T_Stmt], scopedEl: Set[T_Name]) extends T_Stmt
case class T_While(cond: T_Expr, body: List[T_Stmt], scoped: Set[T_Name]) extends T_Stmt
case class T_CodeBlock(body: List[T_Stmt], scoped: Set[T_Name]) extends T_Stmt
case class T_Skip() extends T_Stmt
