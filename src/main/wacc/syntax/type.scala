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

object ArrayType extends ParserBridgePos1[Type, ArrayType] {
    /* for testing */
    def apply(t: Type): ArrayType = ArrayType(t)((0, 0))
    override def labels = List("array type")
}
object PairType extends ParserBridgePos2[Type, Type, PairType] {
    /* for testing */
    def apply(t1: Type, t2: Type): PairType = PairType(t1, t2)((0, 0))
    override def labels = List("pair type")
}

object ErasedPairType extends Type
