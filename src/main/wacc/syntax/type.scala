package wacc.syntax

sealed trait Type

enum BaseType extends Type {
    case Int
    case Bool
    case Char
    case String
}

case class ArrayType(t: Type) extends Type

case class PairType(t1: Type, t2: Type) extends Type

object ErasedPairType extends Type
