package wacc.syntax

sealed trait Type
sealed trait PairElemType

enum BaseType extends Type, PairElemType {
    case IntType
    case BoolType
    case CharType
    case StringType
}

case class ArrayType(t: Type) extends Type, PairElemType

case class PairType(t1: PairElemType, t2: PairElemType) extends Type

object ErasedPairType extends PairElemType
