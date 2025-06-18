package wacc.q_ast

/**
  * Qualified AST for Statements.
  */
sealed trait Q_Stmt

/**
  * Declarations and Assignments.
  * @param v Name of the variable for a declaration.
  * @param r Right hand side of the assignment.
  * @param l Left hand side of the assignment.
  * @param pos position information (line, char) used for locating errors.
  */
case class Q_Decl(v: Name, r: Q_RValue, pos: (Int, Int) = (0, 0)) extends Q_Stmt
case class Q_Asgn(l: Q_LValue, r: Q_RValue, pos: (Int, Int) = (0, 0)) extends Q_Stmt

/**
  * Standard library functions.
  * @param x Expression to be passed in.
  * @param pos position information (line, char) used for locating errors.
  */
case class Q_Free(x: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_Stmt
case class Q_Return(x: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_Stmt
case class Q_Exit(x: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_Stmt
case class Q_Print(x: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_Stmt
case class Q_Println(x: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_Stmt

/**
  * Read function call, only takes in an l value.
  * @param l the value to be read into (ident/pairElem/arrayElem).
  * @param pos position information (line, char) used for locating errors.
  */
case class Q_Read(l: Q_LValue, pos: (Int, Int) = (0, 0)) extends Q_Stmt

/**
  * @param cond Condition of the if statement.
  * @param body Body of the if statement.
  * @param scopedBody Set of variables that are in scope in the body of the if statement.
  * @param el Else body of the if statement.
  * @param scopedEl Set of variables that are in scope in the else body of the if statement.
  * @param scoped Set of variables that are in scope in the while loop.
  * @param pos position information (line, char) used for locating errors.
  */
case class Q_If(cond: Q_Expr, body: List[Q_Stmt], scopedBody: Set[Name], el: List[Q_Stmt], scopedEl: Set[Name], pos: (Int, Int) = (0, 0)) extends Q_Stmt
case class Q_While(cond: Q_Expr, body: List[Q_Stmt], scoped: Set[Name], pos: (Int, Int) = (0, 0)) extends Q_Stmt
case class Q_CodeBlock(body: List[Q_Stmt], scoped: Set[Name], pos: (Int, Int) = (0, 0)) extends Q_Stmt

/**
  * Skip statement.
  * @param pos position information (line, char) used for locating errors.
  */ 
case class Q_Skip(pos: (Int, Int) = (0, 0)) extends Q_Stmt
