package wacc.syntax

sealed trait Type

enum BaseType extends Type {
    case Int
    case Bool
    case Char
    case String
}

case class ArrayType(t: Type)(val pos: (Int, Int)) extends Type
case class PairType(t1: Type, t2: Type)(val pos: (Int, Int)) extends Type

object ArrayType extends ParserBridgePos1[Type, ArrayType]
object PairType extends ParserBridgePos2[Type, Type, PairType]

object ErasedPairType extends Type
