sealed trait Expr extends RValue

sealed trait LValue
sealed trait RValue

// Binary operators
case class Mul(x: Expr, y: Expr) extends Expr
case class Div(x: Expr, y: Expr) extends Expr
case class Mod(x: Expr, y: Expr) extends Expr
case class Add(x: Expr, y: Expr) extends Expr
case class Sub(x: Expr, y: Expr) extends Expr
case class GreaterThan(x: Expr, y: Expr) extends Expr
case class GreaterThanEq(x: Expr, y: Expr) extends Expr
case class LessThan(x: Expr, y: Expr) extends Expr
case class LessThanEq(x: Expr, y: Expr) extends Expr
case class Eq(x: Expr, y: Expr) extends Expr
case class NotEq(x: Expr, y: Expr) extends Expr
case class And(x: Expr, y: Expr) extends Expr
case class Or(x: Expr, y: Expr) extends Expr

// Unary operators
case class Not(x: Expr) extends Expr
case class Neg(x: Expr) extends Expr
case class Len(x: Expr) extends Expr
case class Ord(x: Expr) extends Expr
case class Chr(x: Expr) extends Expr

// Atoms
case class IntLiteral(v: Int) extends Expr
case class BoolLiteral(v: Boolean) extends Expr
case class CharLiteral(v: Char) extends Expr
case class StringLiteral(v: String) extends Expr
object PairNullLiteral extends Expr
case class Ident(v: String) extends Expr, LValue
case class ArrayElem(v: String, is: List[Expr]) extends Expr, LValue

// RValues
case class FuncCall(v: String, args: List[Expr]) extends RValue
case class ArrayLiteral(xs: List[Expr]) extends RValue
case class PairElem(index: String, v: LValue) extends LValue, RValue
case class NewPair(x1: Expr, x2: Expr) extends RValue

