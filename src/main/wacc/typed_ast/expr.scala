package wacc.t_ast

import wacc.ast.PairIndex

sealed trait Q_Expr extends Q_RValue
sealed trait Q_LValue
sealed trait Q_RValue

case class Q_Name(name: String, num: Int)

// Binary operators
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

// Unary operators
sealed trait Q_UnaryOper extends Q_Expr
case class Q_Not(x: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_UnaryOper
case class Q_Neg(x: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_UnaryOper
case class Q_Len(x: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_UnaryOper
case class Q_Ord(x: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_UnaryOper
case class Q_Chr(x: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_UnaryOper

// Atoms
case class Q_IntLiteral(v: BigInt, pos: (Int, Int) = (0, 0)) extends Q_Expr
case class Q_BoolLiteral(v: Boolean, pos: (Int, Int) = (0, 0)) extends Q_Expr
case class Q_CharLiteral(v: Char, pos: (Int, Int) = (0, 0)) extends Q_Expr
case class Q_StringLiteral(v: String, pos: (Int, Int) = (0, 0)) extends Q_Expr
case class Q_Ident(v: Q_Name, pos: (Int, Int) = (0, 0)) extends Q_Expr, Q_LValue
case class Q_ArrayElem(v: Q_Name, indicies: List[Q_Expr], pos: (Int, Int) = (0, 0)) extends Q_Expr, Q_LValue
object Q_PairNullLiteral extends Q_Expr

// RValues
case class Q_FuncCall(v: Q_Name, args: List[Q_Expr], pos: (Int, Int) = (0, 0)) extends Q_RValue
case class Q_ArrayLiteral(xs: List[Q_Expr], pos: (Int, Int) = (0, 0)) extends Q_RValue // TODO: Add type
case class Q_PairElem(index: PairIndex, v: Q_LValue, pos: (Int, Int) = (0, 0)) extends Q_Expr, Q_LValue
case class Q_NewPair(x1: Q_Expr, x2: Q_Expr, pos: (Int, Int) = (0, 0)) extends Q_RValue
