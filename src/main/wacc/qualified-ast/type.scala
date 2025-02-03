package wacc.q_ast

import parsley.generic

sealed trait Q_Type

enum Q_BaseType extends Q_Type {
    case Int
    case Bool
    case Char
    case String
}

case class Q_ArrayType(t: Q_Type) extends Q_Type
case class Q_PairType(t1: Q_Type, t2: Q_Type) extends Q_Type

object Q_ArrayType extends generic.ParserBridge1[Q_Type, Q_ArrayType]
object Q_PairType extends generic.ParserBridge2[Q_Type, Q_Type, Q_PairType]

object Q_ErasedPairType extends Q_Type
