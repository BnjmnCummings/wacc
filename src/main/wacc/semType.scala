package wacc

import wacc.ast.*

sealed abstract class SemType
case object ? extends SemType
// ? represents an unknown type
// This type checks with anything so can be used to replace errors to find other errors

enum KnownType extends SemType {
    case Int
    case Boolean
    case Char
    case String
    case Array(ty: SemType)
    case Pair(ty1: SemType, ty2: SemType)
    case Ident
    // Note an erased pair can be created with ty1/ty2 = pair(?, ?)
}

def toSemType(t: Type): SemType = t match
    case BaseType.Int => KnownType.Int
    case BaseType.Bool => KnownType.Boolean
    case BaseType.Char => KnownType.Char
    case BaseType.String => KnownType.String
    case ArrayType(t) => KnownType.Array(toSemType(t))
    case PairType(t1, t2) => KnownType.Pair(toSemType(t1), toSemType(t2))
    case ErasedPairType => KnownType.Pair(?, ?)

// The typer will take this in
// It maps variable names & function names to their types
// This should allow for variables and functions to share names
class TypeInfo(
    var varTys: Map[String, KnownType],
    var funcTys: Map[(String, KnownType), Map[String, KnownType]]
    // This is a map from (Function Identifier, Return Type) -> parameter map [Param name -> Param type]
)