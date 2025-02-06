package wacc.semantic

import wacc.*
import wacc.ast.*
import wacc.q_ast.*

sealed abstract class TypedExpr extends TypedRValue

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

    case class IntLiteral() extends TypedExpr {
        def ty = KnownType.Int
    }
    case class BoolLiteral() extends TypedExpr {
        def ty = KnownType.Boolean
    }
    case class CharLiteral() extends TypedExpr {
        def ty = KnownType.Char
    }
    case class StringLiteral() extends TypedExpr {
        def ty = KnownType.String
    }
    case class ArrayLiteral(t: SemType) extends TypedExpr {
        def ty = KnownType.Array
    }
    case class PairLiteral(t1: SemType, t2: SemType) extends TypedExpr {
        def ty = KnownType.Pair
    }
    case class Ident() extends TypedExpr, TypedLValue {
        def ty = KnownType.String
    }
    
    case class ArrayElem() extends TypedExpr, TypedLValue {
        def ty = KnownType.Array
    }
}
object TPairNullLiteral extends TypedExpr

sealed trait TypedLValue
sealed trait TypedRValue

sealed abstract class TypedStmt

object TypedStmt {
    case class Decl(v: TypedExpr.Ident, r: TypedRValue) extends TypedStmt
    case class Asgn(l: TypedLValue, r: TypedRValue) extends TypedStmt
    case class Read(l: TypedLValue) extends TypedStmt
    case class Free(x: TypedExpr) extends TypedStmt
    case class Return(x: TypedExpr) extends TypedStmt
    case class Exit(x: TypedExpr) extends TypedStmt
    case class Print(x: TypedExpr) extends TypedStmt
    case class Println(x: TypedExpr) extends TypedStmt
    case class If(cond: TypedExpr, body: List[TypedStmt], el: List[TypedStmt]) extends TypedStmt
    case class While(cond: TypedExpr, body: List[TypedStmt]) extends TypedStmt
    case class CodeBlock(body: List[TypedStmt]) extends TypedStmt
}
object TSkip extends TypedStmt

object TypedRValue {
    case class FuncCall(v: String, args: List[TypedExpr]) extends TypedRValue
    case class ArrayLiteral(xs: List[TypedExpr], t: SemType) extends TypedRValue
    case class PairElem(index: PairIndex, v: TypedLValue) extends TypedExpr, TypedLValue
    case class NewPair(x1: TypedExpr, x2: TypedExpr) extends TypedRValue
}

def knownToTypedExpr(kt: KnownType): TypedExpr = kt match {
    case KnownType.Int => TypedExpr.IntLiteral()
    case KnownType.Boolean => TypedExpr.BoolLiteral()
    case KnownType.Char => TypedExpr.CharLiteral()
    case KnownType.String => TypedExpr.StringLiteral()
    case KnownType.Array(ty) => TypedExpr.ArrayLiteral(ty)
    case KnownType.Pair(ty1, ty2) => TypedExpr.PairLiteral(ty1, ty2)
    case KnownType.Ident => TypedExpr.Ident()
}