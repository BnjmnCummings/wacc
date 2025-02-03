package wacc.q_ast

import wacc.ast.PairIndex
import parsley.generic

sealed trait Q_Expr extends Q_RValue
sealed trait Q_LValue
sealed trait Q_RValue

// Binary operators
sealed trait Q_BinaryOper extends Q_Expr
case class Q_Mul(x: Q_Expr, y: Q_Expr) extends Q_BinaryOper
case class Q_Div(x: Q_Expr, y: Q_Expr) extends Q_BinaryOper
case class Q_Mod(x: Q_Expr, y: Q_Expr) extends Q_BinaryOper
case class Q_Add(x: Q_Expr, y: Q_Expr) extends Q_BinaryOper
case class Q_Sub(x: Q_Expr, y: Q_Expr) extends Q_BinaryOper
case class Q_GreaterThan(x: Q_Expr, y: Q_Expr) extends Q_BinaryOper
case class Q_GreaterThanEq(x: Q_Expr, y: Q_Expr) extends Q_BinaryOper
case class Q_LessThan(x: Q_Expr, y: Q_Expr) extends Q_BinaryOper
case class Q_LessThanEq(x: Q_Expr, y: Q_Expr) extends Q_BinaryOper
case class Q_Eq(x: Q_Expr, y: Q_Expr) extends Q_BinaryOper
case class Q_NotEq(x: Q_Expr, y: Q_Expr) extends Q_BinaryOper
case class Q_And(x: Q_Expr, y: Q_Expr) extends Q_BinaryOper
case class Q_Or(x: Q_Expr, y: Q_Expr) extends Q_BinaryOper

object Q_Mul extends generic.ParserBridge2[Q_Expr, Q_Expr, Q_Mul]
object Q_Div extends generic.ParserBridge2[Q_Expr, Q_Expr, Q_Div]
object Q_Mod extends generic.ParserBridge2[Q_Expr, Q_Expr, Q_Mod]
object Q_Add extends generic.ParserBridge2[Q_Expr, Q_Expr, Q_Add]
object Q_Sub extends generic.ParserBridge2[Q_Expr, Q_Expr, Q_Sub]
object Q_GreaterThan extends generic.ParserBridge2[Q_Expr, Q_Expr, Q_GreaterThan]
object Q_GreaterThanEq extends generic.ParserBridge2[Q_Expr, Q_Expr, Q_GreaterThanEq]
object Q_LessThan extends generic.ParserBridge2[Q_Expr, Q_Expr, Q_LessThan]
object Q_LessThanEq extends generic.ParserBridge2[Q_Expr, Q_Expr, Q_LessThanEq]
object Q_Eq extends generic.ParserBridge2[Q_Expr, Q_Expr, Q_Eq]
object Q_NotEq extends generic.ParserBridge2[Q_Expr, Q_Expr, Q_NotEq]
object Q_And extends generic.ParserBridge2[Q_Expr, Q_Expr, Q_And]
object Q_Or extends generic.ParserBridge2[Q_Expr, Q_Expr, Q_Or]

// Unary operators
sealed trait Q_UnaryOper extends Q_Expr
case class Q_Not(x: Q_Expr) extends Q_UnaryOper
case class Q_Neg(x: Q_Expr) extends Q_UnaryOper
case class Q_Len(x: Q_Expr) extends Q_UnaryOper
case class Q_Ord(x: Q_Expr) extends Q_UnaryOper
case class Q_Chr(x: Q_Expr) extends Q_UnaryOper

object Q_Not extends generic.ParserBridge1[Q_Expr, Q_Not]
object Q_Neg extends generic.ParserBridge1[Q_Expr, Q_Neg]
object Q_Len extends generic.ParserBridge1[Q_Expr, Q_Len]
object Q_Ord extends generic.ParserBridge1[Q_Expr, Q_Ord]
object Q_Chr extends generic.ParserBridge1[Q_Expr, Q_Chr]

// Atoms
case class Q_IntLiteral(v: BigInt) extends Q_Expr
case class Q_BoolLiteral(v: Boolean) extends Q_Expr
case class Q_CharLiteral(v: Char) extends Q_Expr
case class Q_StringLiteral(v: String) extends Q_Expr
case class Q_Ident(v: String) extends Q_Expr, Q_LValue
case class Q_ArrayElem(v: String, indicies: List[Q_Expr]) extends Q_Expr, Q_LValue
object Q_PairNullLiteral extends Q_Expr

object Q_IntLiteral extends generic.ParserBridge1[BigInt, Q_IntLiteral]
object Q_BoolLiteral extends generic.ParserBridge1[Boolean, Q_BoolLiteral]
object Q_CharLiteral extends generic.ParserBridge1[Char, Q_CharLiteral]
object Q_StringLiteral extends generic.ParserBridge1[String, Q_StringLiteral]
object Q_Ident extends generic.ParserBridge1[String, Q_Ident]
object Q_ArrayElem extends generic.ParserBridge2[String, List[Q_Expr], Q_ArrayElem]

// RValues
case class Q_FuncCall(v: String, args: List[Q_Expr]) extends Q_RValue
case class Q_ArrayLiteral(xs: List[Q_Expr]) extends Q_RValue
case class Q_PairElem(index: PairIndex, v: Q_LValue) extends Q_Expr, Q_LValue
case class Q_NewPair(x1: Q_Expr, x2: Q_Expr) extends Q_RValue

object Q_FuncCall extends generic.ParserBridge2[String, List[Q_Expr], Q_FuncCall]
object Q_ArrayLiteral extends generic.ParserBridge1[List[Q_Expr], Q_ArrayLiteral]
object Q_PairElem extends generic.ParserBridge2[PairIndex, Q_LValue, Q_PairElem]
object Q_NewPair extends generic.ParserBridge2[Q_Expr, Q_Expr, Q_NewPair]
