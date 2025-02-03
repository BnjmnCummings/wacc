package wacc.syntax

sealed trait Expr extends RValue
sealed trait LValue
sealed trait RValue

// Binary operators
sealed trait BinaryOper extends Expr
case class Mul(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinaryOper
case class Div(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinaryOper
case class Mod(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinaryOper
case class Add(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinaryOper
case class Sub(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinaryOper
case class GreaterThan(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinaryOper
case class GreaterThanEq(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinaryOper
case class LessThan(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinaryOper
case class LessThanEq(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinaryOper
case class Eq(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinaryOper
case class NotEq(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinaryOper
case class And(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinaryOper
case class Or(x: Expr, y: Expr)(val pos: (Int, Int)) extends BinaryOper

object Mul extends ParserBridgePos2[Expr, Expr, Mul]
object Div extends ParserBridgePos2[Expr, Expr, Div]
object Mod extends ParserBridgePos2[Expr, Expr, Mod]
object Add extends ParserBridgePos2[Expr, Expr, Add]
object Sub extends ParserBridgePos2[Expr, Expr, Sub]
object GreaterThan extends ParserBridgePos2[Expr, Expr, GreaterThan]
object GreaterThanEq extends ParserBridgePos2[Expr, Expr, GreaterThanEq]
object LessThan extends ParserBridgePos2[Expr, Expr, LessThan]
object LessThanEq extends ParserBridgePos2[Expr, Expr, LessThanEq]
object Eq extends ParserBridgePos2[Expr, Expr, Eq]
object NotEq extends ParserBridgePos2[Expr, Expr, NotEq]
object And extends ParserBridgePos2[Expr, Expr, And]
object Or extends ParserBridgePos2[Expr, Expr, Or]

// Unary operators
sealed trait UnaryOper extends Expr
case class Not(x: Expr)(val pos: (Int, Int)) extends UnaryOper
case class Neg(x: Expr)(val pos: (Int, Int)) extends UnaryOper
case class Len(x: Expr)(val pos: (Int, Int)) extends UnaryOper
case class Ord(x: Expr)(val pos: (Int, Int)) extends UnaryOper
case class Chr(x: Expr)(val pos: (Int, Int)) extends UnaryOper

object Not extends ParserBridgePos1[Expr, Not]
object Neg extends ParserBridgePos1[Expr, Neg]
object Len extends ParserBridgePos1[Expr, Len]
object Ord extends ParserBridgePos1[Expr, Ord]
object Chr extends ParserBridgePos1[Expr, Chr]

// Atoms
case class IntLiteral(v: BigInt)(val pos: (Int, Int))extends Expr
case class BoolLiteral(v: Boolean)(val pos: (Int, Int)) extends Expr
case class CharLiteral(v: Char)(val pos: (Int, Int)) extends Expr
case class StringLiteral(v: String)(val pos: (Int, Int)) extends Expr
case class Ident(v: String)(val pos: (Int, Int)) extends Expr, LValue
case class ArrayElem(v: String, indicies: List[Expr])(val pos: (Int, Int)) extends Expr, LValue
object PairNullLiteral extends Expr

object IntLiteral extends ParserBridgePos1[BigInt, IntLiteral]
object BoolLiteral extends ParserBridgePos1[Boolean, BoolLiteral]
object CharLiteral extends ParserBridgePos1[Char, CharLiteral]
object StringLiteral extends ParserBridgePos1[String, StringLiteral]
object Ident extends ParserBridgePos1[String, Ident]
object ArrayElem extends ParserBridgePos2[String, List[Expr], ArrayElem]

// RValues
case class FuncCall(v: String, args: List[Expr])(val pos: (Int, Int)) extends RValue
case class ArrayLiteral(xs: List[Expr])(val pos: (Int, Int)) extends RValue
case class PairElem(index: PairIndex, v: LValue)(val pos: (Int, Int)) extends Expr, LValue
case class NewPair(x1: Expr, x2: Expr)(val pos: (Int, Int)) extends RValue

enum PairIndex {
  case First
  case Second
}

object FuncCall extends ParserBridgePos2[String, List[Expr], FuncCall]
object ArrayLiteral extends ParserBridgePos1[List[Expr], ArrayLiteral]
object PairElem extends ParserBridgePos2[PairIndex, LValue, PairElem]
object NewPair extends ParserBridgePos2[Expr, Expr, NewPair]
