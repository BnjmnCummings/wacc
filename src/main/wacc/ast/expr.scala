package wacc.ast

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

object Mul extends ParserBridgePos2[Expr, Expr, Mul] {
  def apply(x: Expr, y: Expr): Mul = Mul(x, y)((0, 0))
}
object Div extends ParserBridgePos2[Expr, Expr, Div] {
  def apply(x: Expr, y: Expr): Div = Div(x, y)((0, 0))
}
object Mod extends ParserBridgePos2[Expr, Expr, Mod] {
  def apply(x: Expr, y: Expr): Mod = Mod(x, y)((0, 0))
}
object Add extends ParserBridgePos2[Expr, Expr, Add] {
  def apply(x: Expr, y: Expr): Add = Add(x, y)((0, 0))
}
object Sub extends ParserBridgePos2[Expr, Expr, Sub] {
  def apply(x: Expr, y: Expr): Sub = Sub(x, y)((0, 0))
}
object GreaterThan extends ParserBridgePos2[Expr, Expr, GreaterThan] {
  def apply(x: Expr, y: Expr): GreaterThan = GreaterThan(x, y)((0, 0))
}
object GreaterThanEq extends ParserBridgePos2[Expr, Expr, GreaterThanEq] {
  def apply(x: Expr, y: Expr): GreaterThanEq = GreaterThanEq(x, y)((0, 0))
}
object LessThan extends ParserBridgePos2[Expr, Expr, LessThan] {
  def apply(x: Expr, y: Expr): LessThan = LessThan(x, y)((0, 0))
}
object LessThanEq extends ParserBridgePos2[Expr, Expr, LessThanEq] {
  def apply(x: Expr, y: Expr): LessThanEq = LessThanEq(x, y)((0, 0))
}
object Eq extends ParserBridgePos2[Expr, Expr, Eq] {
  def apply(x: Expr, y: Expr): Eq = Eq(x, y)((0, 0))
}
object NotEq extends ParserBridgePos2[Expr, Expr, NotEq] {
  def apply(x: Expr, y: Expr): NotEq = NotEq(x, y)((0, 0))
}
object And extends ParserBridgePos2[Expr, Expr, And] {
  def apply(x: Expr, y: Expr): And = And(x, y)((0, 0))
}
object Or extends ParserBridgePos2[Expr, Expr, Or] {
  def apply(x: Expr, y: Expr): Or = Or(x, y)((0, 0))
}

// Unary operators
sealed trait UnaryOper extends Expr
case class Not(x: Expr)(val pos: (Int, Int)) extends UnaryOper
case class Neg(x: Expr)(val pos: (Int, Int)) extends UnaryOper
case class Len(x: Expr)(val pos: (Int, Int)) extends UnaryOper
case class Ord(x: Expr)(val pos: (Int, Int)) extends UnaryOper
case class Chr(x: Expr)(val pos: (Int, Int)) extends UnaryOper

object Not extends ParserBridgePos1[Expr, Not] {
  def apply(x: Expr): Not = Not(x)((0, 0))
}
object Neg extends ParserBridgePos1[Expr, Neg] {
  def apply(x: Expr): Neg = Neg(x)((0, 0))
}
object Len extends ParserBridgePos1[Expr, Len] {
  def apply(x: Expr): Len = Len(x)((0, 0))
}
object Ord extends ParserBridgePos1[Expr, Ord] {
  def apply(x: Expr): Ord = Ord(x)((0, 0))
}
object Chr extends ParserBridgePos1[Expr, Chr] {
  def apply(x: Expr): Chr = Chr(x)((0, 0))
}

// Atoms
case class IntLiteral(v: BigInt)(val pos: (Int, Int))extends Expr
case class BoolLiteral(v: Boolean)(val pos: (Int, Int)) extends Expr
case class CharLiteral(v: Char)(val pos: (Int, Int)) extends Expr
case class StringLiteral(v: String)(val pos: (Int, Int)) extends Expr
case class Ident(v: String)(val pos: (Int, Int)) extends Expr, LValue
case class ArrayElem(v: String, indicies: List[Expr])(val pos: (Int, Int)) extends Expr, LValue
object PairNullLiteral extends Expr

object IntLiteral extends ParserBridgePos1[BigInt, IntLiteral] {
  def apply(v: BigInt): IntLiteral = IntLiteral(v)((0, 0))
  override def labels = List("integer")
}
object BoolLiteral extends ParserBridgePos1[Boolean, BoolLiteral] {
  def apply(v: Boolean): BoolLiteral = BoolLiteral(v)((0, 0))
  override def labels = List("boolean")
}
object CharLiteral extends ParserBridgePos1[Char, CharLiteral] {
  def apply(v: Char): CharLiteral = CharLiteral(v)((0, 0))
  override def labels = List("character")
}
object StringLiteral extends ParserBridgePos1[String, StringLiteral] {
  def apply(v: String): StringLiteral = StringLiteral(v)((0, 0))
  override def labels = List("string")
}
object Ident extends ParserBridgePos1[String, Ident] {
  def apply(v: String): Ident = Ident(v)((0, 0))
}
object ArrayElem extends ParserBridgePos2[String, List[Expr], ArrayElem] {
  def apply(v: String, indicies: List[Expr]): ArrayElem = ArrayElem(v, indicies)((0, 0))
  override def labels = List("array element")
}

// RValues
case class FuncCall(v: String, args: List[Expr])(val pos: (Int, Int)) extends RValue
case class ArrayLiteral(xs: List[Expr])(val pos: (Int, Int)) extends RValue
case class PairElem(index: PairIndex, v: LValue)(val pos: (Int, Int)) extends Expr, LValue
case class NewPair(x1: Expr, x2: Expr)(val pos: (Int, Int)) extends RValue

enum PairIndex {
  case First
  case Second
}

object FuncCall extends ParserBridgePos2[String, List[Expr], FuncCall] {
  def apply(v: String, args: List[Expr]): FuncCall = FuncCall(v, args)((0, 0))
  override def labels = List("function call")
}
object ArrayLiteral extends ParserBridgePos1[List[Expr], ArrayLiteral] {
  def apply(xs: List[Expr]): ArrayLiteral = ArrayLiteral(xs)((0, 0))
  override def labels = List("array")
}
object PairElem extends ParserBridgePos2[PairIndex, LValue, PairElem] {
  def apply(index: PairIndex, v: LValue): PairElem = PairElem(index, v)((0, 0))
  override def labels = List("pair element")
}
object NewPair extends ParserBridgePos2[Expr, Expr, NewPair] {
  def apply(x1: Expr, x2: Expr): NewPair = NewPair(x1, x2)((0, 0))
  override def labels = List("pair constructor")
}
