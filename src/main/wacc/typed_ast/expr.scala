package wacc.t_ast

import wacc.ast.PairIndex

sealed trait T_Expr extends T_RValue
sealed trait T_LValue
sealed trait T_RValue

case class T_Name(name: String, num: Int)

// Binary operators
sealed trait T_BinaryOper extends T_Expr
case class T_Mul(x: T_Expr, y: T_Expr, pos: (Int, Int) = (0, 0)) extends T_BinaryOper
case class T_Div(x: T_Expr, y: T_Expr, pos: (Int, Int) = (0, 0)) extends T_BinaryOper
case class T_Mod(x: T_Expr, y: T_Expr, pos: (Int, Int) = (0, 0)) extends T_BinaryOper
case class T_Add(x: T_Expr, y: T_Expr, pos: (Int, Int) = (0, 0)) extends T_BinaryOper
case class T_Sub(x: T_Expr, y: T_Expr, pos: (Int, Int) = (0, 0)) extends T_BinaryOper
case class T_GreaterThan(x: T_Expr, y: T_Expr, pos: (Int, Int) = (0, 0)) extends T_BinaryOper
case class T_GreaterThanEq(x: T_Expr, y: T_Expr, pos: (Int, Int) = (0, 0)) extends T_BinaryOper
case class T_LessThan(x: T_Expr, y: T_Expr, pos: (Int, Int) = (0, 0)) extends T_BinaryOper
case class T_LessThanEq(x: T_Expr, y: T_Expr, pos: (Int, Int) = (0, 0)) extends T_BinaryOper
case class T_Eq(x: T_Expr, y: T_Expr, pos: (Int, Int) = (0, 0)) extends T_BinaryOper
case class T_NotEq(x: T_Expr, y: T_Expr, pos: (Int, Int) = (0, 0)) extends T_BinaryOper
case class T_And(x: T_Expr, y: T_Expr, pos: (Int, Int) = (0, 0)) extends T_BinaryOper
case class T_Or(x: T_Expr, y: T_Expr, pos: (Int, Int) = (0, 0)) extends T_BinaryOper

// Unary operators
sealed trait T_UnaryOper extends T_Expr
case class T_Not(x: T_Expr, pos: (Int, Int) = (0, 0)) extends T_UnaryOper
case class T_Neg(x: T_Expr, pos: (Int, Int) = (0, 0)) extends T_UnaryOper
case class T_Len(x: T_Expr, pos: (Int, Int) = (0, 0)) extends T_UnaryOper
case class T_Ord(x: T_Expr, pos: (Int, Int) = (0, 0)) extends T_UnaryOper
case class T_Chr(x: T_Expr, pos: (Int, Int) = (0, 0)) extends T_UnaryOper

// Atoms
case class T_IntLiteral(v: BigInt, pos: (Int, Int) = (0, 0)) extends T_Expr
case class T_BoolLiteral(v: Boolean, pos: (Int, Int) = (0, 0)) extends T_Expr
case class T_CharLiteral(v: Char, pos: (Int, Int) = (0, 0)) extends T_Expr
case class T_StringLiteral(v: String, pos: (Int, Int) = (0, 0)) extends T_Expr
case class T_Ident(v: T_Name, pos: (Int, Int) = (0, 0)) extends T_Expr, T_LValue
case class T_ArrayElem(v: T_Name, indicies: List[T_Expr], pos: (Int, Int) = (0, 0)) extends T_Expr, T_LValue
object T_PairNullLiteral extends T_Expr

// RValues
case class T_FuncCall(v: T_Name, args: List[T_Expr], pos: (Int, Int) = (0, 0)) extends T_RValue
case class T_ArrayLiteral(xs: List[T_Expr], pos: (Int, Int) = (0, 0)) extends T_RValue // TODO: Add type
case class T_PairElem(index: PairIndex, v: T_LValue, pos: (Int, Int) = (0, 0)) extends T_Expr, T_LValue
case class T_NewPair(x1: T_Expr, x2: T_Expr, pos: (Int, Int) = (0, 0)) extends T_RValue
