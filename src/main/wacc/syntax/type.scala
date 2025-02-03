package wacc.ast

import parsley.generic

sealed trait Type

enum BaseType extends Type {
    case Int
    case Bool
    case Char
    case String
}

case class ArrayType(t: Type) extends Type
case class PairType(t1: Type, t2: Type) extends Type

object ArrayType extends generic.ParserBridge1[Type, ArrayType]
object PairType extends generic.ParserBridge2[Type, Type, PairType]

object ErasedPairType extends Type
