sealed trait Expr 

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
case class Ident(v: String) extends Expr
case class ArrayElem(v: Ident, is: List[Expr]) extends Expr

/*
  sealed abstract class CharLiteral extends Expr
  case class EscapedCharLiteral(v: Char) extends CharLiteral
  case class StandardCharLiteral(v: Char) extends CharLiteral
  case class StringLiteral(v: List[Char]) extends Expr
*/