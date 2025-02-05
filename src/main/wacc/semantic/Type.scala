package wacc.semantic

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