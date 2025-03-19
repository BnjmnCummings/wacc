package wacc.t_ast

import wacc.ast.PairIndex
import wacc.semantic.SemType
import wacc.q_ast.Name

sealed trait T_LValue
sealed trait T_RValue
sealed trait T_Expr extends T_RValue

/**
  * A typed function call.
  * The type can be evaluated by dereferencing the name v.
  * @param v the name of the function.
  * @param args the arguments to pass in.
  */
case class T_FuncCall(v: Name, args: List[T_Expr]) extends T_RValue

/**
  * A typed array literal.
  * @param xs the list of values stored in the array.
  * @param ty the type of each value.
  * @param length the length of the array.
  */
case class T_ArrayLiteral(xs: List[T_Expr], ty: SemType, length: Int) extends T_RValue 

/**
  * A typed pair element.
  * @param index the pair index: fst or snd
  * @param v the l-value to be queried. Must evaluate to a [[PairType]]
  */
case class T_PairElem(index: PairIndex, v: T_LValue) extends T_Expr, T_LValue

/**
  * A typed pair constructor: 'newpair(x1, x2)'.
  * @param x1 the first element.
  * @param x2 the second element.
  * @param ty1 the type of the first element.
  * @param ty2 the type of the second element.
  */
case class T_NewPair(x1: T_Expr, x2: T_Expr, ty1: SemType, ty2: SemType) extends T_RValue 


/**
  * Typed Binary Operator Expressions
  * @param x the first operand.
  * @param y the second operand.
  */
sealed trait T_BinaryOper extends T_Expr
case class T_Mul(x: T_Expr, y: T_Expr) extends T_BinaryOper
case class T_Div(x: T_Expr, y: T_Expr) extends T_BinaryOper
case class T_Mod(x: T_Expr, y: T_Expr) extends T_BinaryOper
case class T_Add(x: T_Expr, y: T_Expr) extends T_BinaryOper
case class T_Sub(x: T_Expr, y: T_Expr) extends T_BinaryOper
case class T_GreaterThan(x: T_Expr, y: T_Expr, ty: SemType) extends T_BinaryOper
case class T_GreaterThanEq(x: T_Expr, y: T_Expr, ty: SemType) extends T_BinaryOper
case class T_LessThan(x: T_Expr, y: T_Expr, ty: SemType) extends T_BinaryOper
case class T_LessThanEq(x: T_Expr, y: T_Expr, ty: SemType) extends T_BinaryOper
case class T_Eq(x: T_Expr, y: T_Expr, ty: SemType) extends T_BinaryOper
case class T_NotEq(x: T_Expr, y: T_Expr, ty: SemType) extends T_BinaryOper
case class T_And(x: T_Expr, y: T_Expr) extends T_BinaryOper
case class T_Or(x: T_Expr, y: T_Expr) extends T_BinaryOper

/**
  * Typed Unary Operator Expressions
  * @param x the operand.
  */
sealed trait T_UnaryOper extends T_Expr
case class T_Not(x: T_Expr) extends T_UnaryOper
case class T_Neg(x: T_Expr) extends T_UnaryOper
case class T_Len(x: T_Expr) extends T_UnaryOper
case class T_Ord(x: T_Expr) extends T_UnaryOper
case class T_Chr(x: T_Expr) extends T_UnaryOper

/**
  * Typed Atomic Expressions
  * @param v the value of the expression 'atom'.
  */
case class T_IntLiteral(v: BigInt) extends T_Expr
case class T_BoolLiteral(v: Boolean) extends T_Expr
case class T_CharLiteral(v: Char) extends T_Expr
case class T_StringLiteral(v: String) extends T_Expr
case class T_Ident(v: Name) extends T_Expr, T_LValue
case class T_ArrayElem(v: Name, indicies: List[T_Expr]) extends T_Expr, T_LValue
object T_PairNullLiteral extends T_Expr
