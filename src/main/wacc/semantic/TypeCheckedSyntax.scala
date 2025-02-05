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
    case class Ident(v: String) extends TypedExpr, TypedLValue {
        def ty = BaseType.String
    }
    case class ArrayElem(v: String, indicies: List[Expr]) extends TypedExpr, TypedLValue {
        def ty = ???
    }
    // needs pair-liter ??
}

sealed trait TypedLValue
sealed trait TypedRValue

sealed abstract class TypedStmt {
    def ty: SemType
}

object TypedStmt {
    case class Decl(t: Type, v: String, r: TypedRValue) extends TypedStmt
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

object TypedRValue {
    case class FuncCall(v: String, args: List[TypedExpr]) extends TypedRValue
    case class ArrayLiteral(xs: List[TypedExpr]) extends TypedRValue
    case class PairElem(index: PairIndex, v: TypedLValue) extends TypedExpr, TypedLValue
    case class NewPair(x1: TypedExpr, x2: TypedExpr) extends TypedRValue
}