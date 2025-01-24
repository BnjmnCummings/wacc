package wacc.syntax

sealed trait Expr extends RValue
sealed trait LValue
sealed trait RValue

// Binary operators
sealed trait BinaryOper extends Expr
case class Mul(x: Expr, y: Expr) extends BinaryOper
case class Div(x: Expr, y: Expr) extends BinaryOper
case class Mod(x: Expr, y: Expr) extends BinaryOper
case class Add(x: Expr, y: Expr) extends BinaryOper
case class Sub(x: Expr, y: Expr) extends BinaryOper
case class GreaterThan(x: Expr, y: Expr) extends BinaryOper
case class GreaterThanEq(x: Expr, y: Expr) extends BinaryOper
case class LessThan(x: Expr, y: Expr) extends BinaryOper
case class LessThanEq(x: Expr, y: Expr) extends BinaryOper
case class Eq(x: Expr, y: Expr) extends BinaryOper
case class NotEq(x: Expr, y: Expr) extends BinaryOper
case class And(x: Expr, y: Expr) extends BinaryOper
case class Or(x: Expr, y: Expr) extends BinaryOper

// Unary operators
sealed trait UnaryOper extends Expr
case class Not(x: Expr) extends UnaryOper
case class Neg(x: Expr) extends UnaryOper
case class Len(x: Expr) extends UnaryOper
case class Ord(x: Expr) extends UnaryOper
case class Chr(x: Expr) extends UnaryOper

// Atoms
case class IntLiteral(v: BigInt) extends Expr
case class BoolLiteral(v: Boolean) extends Expr
sealed abstract class CharLiteral extends Expr
case class EscapedCharLiteral(v: Char) extends CharLiteral
case class StandardCharLiteral(v: Char) extends CharLiteral
case class StringLiteral(v: List[CharLiteral]) extends Expr
case class Ident(v: String) extends Expr, LValue
case class ArrayElem(v: String, is: List[Expr]) extends Expr, LValue

object PairNullLiteral extends Expr

// RValues
case class FuncCall(v: String, args: List[Expr]) extends RValue
case class ArrayLiteral(xs: List[Expr]) extends RValue
case class PairElem(index: PairIndex, v: LValue) extends LValue, RValue
case class NewPair(x1: Expr, x2: Expr) extends RValue

enum PairIndex {
  case First
  case Second
}
