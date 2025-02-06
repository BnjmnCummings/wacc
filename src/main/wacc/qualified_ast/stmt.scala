package wacc.q_ast

import wacc.ast.*
import parsley.generic

sealed trait Q_Stmt

case class Q_Decl(v: Q_Name, r: Q_RValue) extends Q_Stmt
case class Q_Asgn(l: Q_LValue, r: Q_RValue) extends Q_Stmt
case class Q_Read(l: Q_LValue) extends Q_Stmt
case class Q_Free(x: Q_Expr) extends Q_Stmt
case class Q_Return(x: Q_Expr) extends Q_Stmt
case class Q_Exit(x: Q_Expr) extends Q_Stmt
case class Q_Print(x: Q_Expr) extends Q_Stmt
case class Q_Println(x: Q_Expr) extends Q_Stmt
case class Q_If(cond: Q_Expr, body: List[Q_Stmt], scopedBody: Set[Q_Name], el: List[Q_Stmt], scopedEl: Set[Q_Name]) extends Q_Stmt
case class Q_While(cond: Q_Expr, body: List[Q_Stmt], scoped: Set[Q_Name]) extends Q_Stmt
case class Q_CodeBlock(body: List[Q_Stmt], scoped: Set[Q_Name]) extends Q_Stmt

object Q_Decl extends generic.ParserBridge2[Q_Name, Q_RValue, Q_Decl]
object Q_Asgn extends generic.ParserBridge2[Q_LValue, Q_RValue, Q_Asgn]
object Q_Read extends generic.ParserBridge1[Q_LValue, Q_Read]
object Q_Free extends generic.ParserBridge1[Q_Expr, Q_Free]
object Q_Return extends generic.ParserBridge1[Q_Expr, Q_Return]
object Q_Exit extends generic.ParserBridge1[Q_Expr, Q_Exit]
object Q_Print extends generic.ParserBridge1[Q_Expr, Q_Print]
object Q_Println extends generic.ParserBridge1[Q_Expr, Q_Println]
object Q_If extends generic.ParserBridge5[Q_Expr, List[Q_Stmt], Set[Q_Name], List[Q_Stmt], Set[Q_Name], Q_If]
object Q_While extends generic.ParserBridge3[Q_Expr, List[Q_Stmt], Set[Q_Name], Q_While]
object Q_CodeBlock extends generic.ParserBridge2[List[Q_Stmt], Set[Q_Name], Q_CodeBlock]

object Q_Skip extends Q_Stmt
