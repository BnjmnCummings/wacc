package wacc.syntax

import parsley.generic

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

object Mul extends generic.ParserBridge2[Expr, Expr, Mul]
object Div extends generic.ParserBridge2[Expr, Expr, Div]
object Mod extends generic.ParserBridge2[Expr, Expr, Mod]
object Add extends generic.ParserBridge2[Expr, Expr, Add]
object Sub extends generic.ParserBridge2[Expr, Expr, Sub]
object GreaterThan extends generic.ParserBridge2[Expr, Expr, GreaterThan]
object GreaterThanEq extends generic.ParserBridge2[Expr, Expr, GreaterThanEq]
object LessThan extends generic.ParserBridge2[Expr, Expr, LessThan]
object LessThanEq extends generic.ParserBridge2[Expr, Expr, LessThanEq]
object Eq extends generic.ParserBridge2[Expr, Expr, Eq]
object NotEq extends generic.ParserBridge2[Expr, Expr, NotEq]
object And extends generic.ParserBridge2[Expr, Expr, And]
object Or extends generic.ParserBridge2[Expr, Expr, Or]


// Unary operators
sealed trait UnaryOper extends Expr
case class Not(x: Expr) extends UnaryOper
case class Neg(x: Expr) extends UnaryOper
case class Len(x: Expr) extends UnaryOper
case class Ord(x: Expr) extends UnaryOper
case class Chr(x: Expr) extends UnaryOper

object Not extends generic.ParserBridge1[Expr, Not]
object Neg extends generic.ParserBridge1[Expr, Neg]
object Len extends generic.ParserBridge1[Expr, Len]
object Ord extends generic.ParserBridge1[Expr, Ord]
object Chr extends generic.ParserBridge1[Expr, Chr]


// Atoms
case class IntLiteral(v: BigInt) extends Expr
case class BoolLiteral(v: Boolean) extends Expr
sealed abstract class CharLiteral extends Expr
case class EscapedCharLiteral(v: Char) extends CharLiteral
case class StandardCharLiteral(v: Char) extends CharLiteral
case class StringLiteral(v: List[CharLiteral]) extends Expr
case class Ident(v: String) extends Expr, LValue
case class ArrayElem(v: String, indicies: List[Expr]) extends Expr, LValue

object PairNullLiteral extends Expr

object IntLiteral extends generic.ParserBridge1[BigInt, IntLiteral]
object BoolLiteral extends generic.ParserBridge1[Boolean, BoolLiteral]
object EscapedCharLiteral extends generic.ParserBridge1[Char, EscapedCharLiteral]
object StandardCharLiteral extends generic.ParserBridge1[Char, StandardCharLiteral]
object StringLiteral extends generic.ParserBridge1[List[CharLiteral], StringLiteral]
object Ident extends generic.ParserBridge1[String, Ident]
object ArrayElem extends generic.ParserBridge2[String, List[Expr], ArrayElem]


// RValues
case class FuncCall(v: String, args: List[Expr]) extends RValue
case class ArrayLiteral(xs: List[Expr]) extends RValue
case class PairElem(index: PairIndex, v: LValue) extends Expr, LValue
case class NewPair(x1: Expr, x2: Expr) extends RValue

enum PairIndex {
  case First
  case Second
}

object FuncCall extends generic.ParserBridge2[String, List[Expr], FuncCall]
object ArrayLiteral extends generic.ParserBridge1[List[Expr], ArrayLiteral]
object PairElem extends generic.ParserBridge2[PairIndex, LValue, PairElem]
object NewPair extends generic.ParserBridge2[Expr, Expr, NewPair]
