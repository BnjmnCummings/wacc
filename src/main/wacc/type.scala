sealed trait Type
sealed trait PairElemType

enum class BaseType extends Type, PairElemType {
    case IntType
    case BoolType
    case CharType
    case StringType
}

case class ArrayType(t: BaseType) extends Type, PairElemType

case class PairType(t1: PairElemType, t2: PairElemType) extends Type

case class ErasedPairType extends PairElemType