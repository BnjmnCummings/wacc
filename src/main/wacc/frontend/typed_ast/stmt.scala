package wacc.t_ast

import wacc.SemType
import wacc.q_ast.Name

/**
  * Typed AST for Statements.
  */
sealed trait T_Stmt 

/**
  * Declarations and Assignments.
  * @param v Name of the variable for a declaration.
  * @param r Right hand side of the assignment.
  * @param ty Type of the variable.
  * @param l Left hand side of the assignment.
  */
case class T_Decl(v: Name, r: T_RValue, ty: SemType) extends T_Stmt
case class T_Asgn(l: T_LValue, r: T_RValue, ty: SemType) extends T_Stmt

/**
  * Standard library functions.
  * @param x Expression to be passed in.
  * @param ty the type of the expression.
  */
case class T_Free(x: T_Expr, ty: SemType) extends T_Stmt
case class T_Return(x: T_Expr, ty: SemType) extends T_Stmt
case class T_Exit(x: T_Expr) extends T_Stmt
case class T_Print(x: T_Expr, ty: SemType) extends T_Stmt
case class T_Println(x: T_Expr, ty: SemType) extends T_Stmt 

/**
  * Read function call, only takes in an l value.
  * @param l the value to be read into (ident/pairElem/arrayElem).
  * @param ty the type of the value.
  */
case class T_Read(l: T_LValue, ty: SemType) extends T_Stmt 

/**
  * @param cond Condition of the if statement.
  * @param body Body of the if statement.
  * @param scopedBody Set of variables that are in scope in the body of the if statement.
  * @param el Else body of the if statement.
  * @param scopedEl Set of variables that are in scope in the else body of the if statement.
  * @param scoped Set of variables that are in scope in the while loop.
  */
case class T_If(cond: T_Expr, body: List[T_Stmt], scopedBody: Set[Name], el: List[T_Stmt], scopedEl: Set[Name]) extends T_Stmt
case class T_While(cond: T_Expr, body: List[T_Stmt], scoped: Set[Name]) extends T_Stmt
case class T_CodeBlock(body: List[T_Stmt], scoped: Set[Name]) extends T_Stmt

/* Skip statement. */
case class T_Skip() extends T_Stmt
