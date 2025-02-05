package wacc.semantic
import wacc.syntax.*

sealed abstract class TypedExpr extends TypedRValue {
    def ty: Type
}

object TypedExpr {
    case class Mul(x: TypedExpr, y: TypedExpr) extends TypedExpr
    case class Div(x: TypedExpr, y: TypedExpr) extends TypedExpr
    case class Mod(x: TypedExpr, y: TypedExpr) extends TypedExpr
    case class Add(x: TypedExpr, y: TypedExpr) extends TypedExpr
    case class Sub(x: TypedExpr, y: TypedExpr) extends TypedExpr
    case class GreaterThan(x: TypedExpr, y: TypedExpr) extends TypedExpr
    case class GreaterThanEq(x: TypedExpr, y: TypedExpr) extends TypedExpr
    case class LessThan(x: TypedExpr, y: TypedExpr) extends TypedExpr
    case class LessThanEq(x: TypedExpr, y: TypedExpr) extends TypedExpr
    case class Eq(x: TypedExpr, y: TypedExpr) extends TypedExpr
    case class NotEq(x: TypedExpr, y: TypedExpr) extends TypedExpr
    case class And(x: TypedExpr, y: TypedExpr) extends TypedExpr
    case class Or(x: TypedExpr, y: TypedExpr) extends TypedExpr

    case class Not(x: TypedExpr) extends TypedExpr
    case class Neg(x: TypedExpr) extends TypedExpr
    case class Len(x: TypedExpr) extends TypedExpr
    case class Ord(x: TypedExpr) extends TypedExpr
    case class Chr(x: TypedExpr) extends TypedExpr

    case class IntLiteral(v: BigInt) extends TypedExpr {
        def ty = BaseType.Int
    }
    case class BoolLiteral(v: Boolean) extends TypedExpr {
        def ty = BaseType.Bool
    }
    case class EscapedCharLiteral(v: Char) extends TypedExpr {
        def ty = BaseType.Char // check this??
    }
    case class StandardCharLiteral(v: Char) extends TypedExpr {
        def ty = BaseType.Char
    }
    case class StringLiteral(v: List[CharLiteral]) extends TypedExpr {
        def ty = BaseType.String
    }
    case class Ident(v: String) extends TypedExpr, LValue {
        def ty = BaseType.String
    }
    case class ArrayElem(v: String, indicies: List[Expr]) extends TypedExpr, LValue {
        def ty = ???
    }
    // needs pair-liter ??
}