package wacc.q_ast

import wacc.ast.PairIndex

sealed trait Q_LValue
sealed trait Q_RValue
sealed trait Q_Expr extends Q_RValue

/**
  * A qualified function call.
  * @param v the name of the function.
  * @param args the arguments to pass in.
  * @param pos position information (line, char) used for locating errors.
  */
case class Q_FuncCall(v: Name, args: List[Q_Expr], pos: (Int, Int) = (0, 0)) extends Q_RValue

/**
  * A qualified array literal.
  * @param xs the list of values stored in the array.
  * @param pos position information (line, char) used for locating errors.
  */
case class Q_ArrayLiteral(xs: List[Q_Expr], pos: (Int, Int) = (0, 0)) extends Q_RValue

/**
  * A qualified pair element.
  * @param index the pair index: fst or snd
  * @param v the l-value to be queried. Must evaluate to a [[PairType]]
  * @param pos position information (line, char) used for locating errors.
  */
case class Q_PairElem(index: PairIndex, v: Q_LValue, pos: (Int, Int) = (0, 0)) extends Q_Expr, Q_LValue

/**
  * A qualified pair constructor: 'newpair(x1, x2)'.
  * @param x1 the first element.
  * @param x2 the second element.
  * @param pos position information (line, char) used for locating errors.
  */
case class Q_NewPair(x1: Q_Expr, x2: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_RValue


/**
  * Qualified Binary Operator Expressions
  * @param x the first operand.
  * @param y the second operand.
  * @param pos position information (line, char) used for locating errors.
  */
sealed trait Q_BinaryOper extends Q_Expr
case class Q_Mul(x: Q_Expr, y: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_BinaryOper
case class Q_Div(x: Q_Expr, y: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_BinaryOper
case class Q_Mod(x: Q_Expr, y: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_BinaryOper
case class Q_Add(x: Q_Expr, y: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_BinaryOper
case class Q_Sub(x: Q_Expr, y: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_BinaryOper
case class Q_GreaterThan(x: Q_Expr, y: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_BinaryOper
case class Q_GreaterThanEq(x: Q_Expr, y: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_BinaryOper
case class Q_LessThan(x: Q_Expr, y: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_BinaryOper
case class Q_LessThanEq(x: Q_Expr, y: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_BinaryOper
case class Q_Eq(x: Q_Expr, y: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_BinaryOper
case class Q_NotEq(x: Q_Expr, y: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_BinaryOper
case class Q_And(x: Q_Expr, y: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_BinaryOper
case class Q_Or(x: Q_Expr, y: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_BinaryOper

/**
  * Qualified Unary Operator Expressions
  * @param x the operand.
  * @param pos position information (line, char) used for locating errors.
  */
sealed trait Q_UnaryOper extends Q_Expr
case class Q_Not(x: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_UnaryOper
case class Q_Neg(x: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_UnaryOper
case class Q_Len(x: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_UnaryOper
case class Q_Ord(x: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_UnaryOper
case class Q_Chr(x: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_UnaryOper

/**
  * Qualified Atomic Expressions
  * @param v the value of the expression 'atom'.
  * @param pos position information (line, char) used for locating errors.
  */
case class Q_IntLiteral(v: BigInt, pos: (Int, Int) = (0, 0)) extends Q_Expr
case class Q_BoolLiteral(v: Boolean, pos: (Int, Int) = (0, 0)) extends Q_Expr
case class Q_CharLiteral(v: Char, pos: (Int, Int) = (0, 0)) extends Q_Expr
case class Q_StringLiteral(v: String, pos: (Int, Int) = (0, 0)) extends Q_Expr
case class Q_Ident(v: Name, pos: (Int, Int) = (0, 0)) extends Q_Expr, Q_LValue
case class Q_ArrayElem(v: Name, indicies: List[Q_Expr], pos: (Int, Int) = (0, 0)) extends Q_Expr, Q_LValue
object Q_PairNullLiteral extends Q_Expr

/**
  * Case class to represent unique identity names.
  * @param value the 'name' of the variable/function
  * @param num the unique 'id number' for variable shadowing.
  */
case class Name(value: String, num: Int)
