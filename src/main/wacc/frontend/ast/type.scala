package wacc.ast

sealed trait Type

/* Enum for representing the primative types in WACC */
enum BaseType extends Type {
    case Int
    case Bool
    case Char
    case String
}

/**
  * AST Node for an array type in WACC.
  * @param t the type of each element in the array.
  * @param pos position information (line, char) used for locating errors.
  */
case class ArrayType(t: Type)(val pos: (Int, Int)) extends Type

/**
  * AST Node for a pair type in WACC.
  * @param t1 the type of the first element.
  * @param t2 the type of the second element.
  * @param pos position information (line, char) used for locating errors.
  */
case class PairType(t1: Type, t2: Type)(val pos: (Int, Int)) extends Type

/* AST node for representing the 'pair' type with no further type parameters*/
object ErasedPairType extends Type

/**
  * Companion object for default positioning and error labeling.
  */
object ArrayType extends ParserBridgePos1[Type, ArrayType] {
    def apply(t: Type): ArrayType = ArrayType(t)((0, 0))
    override def labels = List("array type")
}
object PairType extends ParserBridgePos2[Type, Type, PairType] {
    def apply(t1: Type, t2: Type): PairType = PairType(t1, t2)((0, 0))
    override def labels = List("pair type")
}


