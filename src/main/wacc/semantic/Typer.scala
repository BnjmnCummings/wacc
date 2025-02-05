package wacc.semantic

import wacc.syntax.* 
import collection.mutable

case class TypedProg(funcs: List[TypedFunc], body: List[TypedStmt])
case class TypedFunc(t: Type, id: Ident, args: List[TypedParam], body: List[TypedStmt])
case class TypedParam(t: Type, id: Ident)

def typeCheck(prog@(funcs: List[Func], body: List): Prog, tyInfo: TypeInfo): Either[List[Error], TypedProg] = { // Note this List is non-empty. Import won't work
    given ctx: TypeCheckerCtx[List[Error]] = TypeCheckerCtx(tyInfo, List.newBuilder)
    val typedProg = prog.map(check) // prog is not a list of stmt?? CHECK THIS!

    ctx.errors.match {
        case err :: errs => Left(err :: errs)
        case Nil         => Right(typedProg)
    }
}

def check(stmt: Stmt)(using TypeCheckerCtx[?]): TypedStmt = stmt match {
    case Decl(t: Type, id: Ident, r: RValue) =>
        // This will check the type of r compared to given type t
        val (_, typedR) = check(r, Constraint.Is(t)) // GIVEN A SYNTAX TYPE, PERHAPS USE THE CTX.var SHIT FOR ITS TYPE RATHER THAN T??
        TypedStmt.Decl(t, id, typedR)
    case Asgn(l: LValue, r: RValue) =>
        // Get the type of left value
        val (ty, typedL) = check(l, Constraint.Unconstrained)
        // We need the type of the right value to match this
        val (_, typedR) = check(r, Constraint.Is(ty.getOrElse(?)))
        TypedStmt.Asgn(typedL, typedR)
    case Read(l: LValue) =>
        // Only need to  verify the LValue is al LValue - no constraint needed?
        val (ty, typedL) = check(l, Constraint.Unconstrained)
        TypedStmt.Read(typedL)
    case Free(x: Expr) =>
        val (ty, typedX) = check(x, Constraint.Unconstrained) // Create a constraint for free values!
        TypedStmt.Free(typedX)
    case Return(x: Expr) =>
        val (ty, typedX) = check(x, Constraint.Unconstrained) // Create a constraint for return values!
        TypedStmt.Return(typedX)
    case Exit(x: Expr) =>
        val (ty, typedX) = check(x, Constraint.Unconstrained) // Create a constraint for exit values!
        TypedStmt.Return(typedX)
    case Print(x: Expr) =>
        val (ty, typedX) = check(x, Constraint.Unconstrained) // Create a constraint for print values!
        TypedStmt.Return(typedX)
    case Println(x: Expr) =>
        val (ty, typedX) = check(x, Constraint.Unconstrained) // Create a constraint for println values!
        TypedStmt.Return(typedX)
    case If(cond: Expr, body: List[Stmt], el: List[Stmt]) =>
        val (condTy, typedCond) = check(cond, Constraint.Unconstrained) // Create a constraint for this being a boolean!
        val (bodyTy, typedBody) = check(body, Constraint.Unconstrained) // Think this can remain as Unconstrained
        val (elTy, typedEl) = check(el, Constraint.Unconstrained) // As above
        TypedStmt.If(typedCond, typedBody, typedEl)
    case While(cond: Expr, body: List[Stmt]) =>
        val (condTy, typedCond) = check(cond, Constraint.Unconstrained) // Create a constraint for this being a boolean!
        val (bodyTy, typedBody) = check(body, Constraint.Unconstrained) // Think this can remain as Unconstrained
        TypedStmt.While(typedCond, typedBody)
    case CodeBlock(body: List[Stmt]) =>
        val (bodyTy, typedBody) = check(bodyTy, Constraint.Unconstrained) // Don't see why this should be anything other than Unconstrained
        TypedStmt.CodeBlock(typedBody)
}

def check(expr: Expr, c: Constraint)(using TypeCheckerCtx[?]): (Option[SemType], TypedExpr) = expr match {
    // The below only works on two ints
    case Mul(x: Expr, y: Expr) => checkArithmeticExpr(x, y, c)(TypedExpr.Mul.apply)
    case Div(x: Expr, y: Expr) => checkArithmeticExpr(x, y, c)(TypedExpr.Div.apply)
    case Mod(x: Expr, y: Expr) => checkArithmeticExpr(x, y, c)(TypedExpr.Mod.apply)
    case Add(x: Expr, y: Expr) => checkArithmeticExpr(x, y, c)(TypedExpr.Add.apply)
    case Sub(x: Expr, y: Expr) => checkArithmeticExpr(x, y, c)(TypedExpr.Div.apply)

    // The below work on two ints or two chars
    case GreaterThan(x: Expr, y: Expr) => checkComparisonExpr(x, y, c)(TypedExpr.GreaterThan.apply)
    case GreaterThanEq(x: Expr, y: Expr) => checkComparisonExpr(x, y, c)(TypedExpr.GreaterThanEq.apply)
    case LessThan(x: Expr, y: Expr) => checkComparisonExpr(x, y, c)(TypedExpr.LessThan.apply)
    case LessThanEq(x: Expr, y: Expr) => checkComparisonExpr(x, y, c)(TypedExpr.LessThanEq.apply)

    // The below work on two of the same type
    case Eq(x: Expr, y: Expr) =>
        val (xTy, typedX) = check(x, Constraint.Is(?))
        val (yTy, typedY) = check(y, Constraint.Is(xTy.getOrElse(?)))
        val ty = mostSpecific(xTy, yTy)
        (ty.satisfies(c), TypedExpr.Eq(typedX, typedY))
    case NotEq(x: Expr, y: Expr) => 
        val (xTy, typedX) = check(x, Constraint.Is(?))
        val (yTy, typedY) = check(y, Constraint.Is(xTy.getOrElse(?)))
        val ty = mostSpecific(xTy, yTy)
        (ty.satisfies(c), TypedExpr.NotEq(typedX, typedY))

    // The below only work on two bools
    case And(x: Expr, y: Expr) => checkBooleanExpr(x, y, c)(TypedExpr.And.apply)
    case Or(x: Expr, y: Expr) => checkBooleanExpr(x, y, c)(TypedExpr.Or.apply)

    // The below are unary
    // Int:
    case Neg(x: Expr) =>
        val (xTy, typedX) = check(x, Constraint.IsNumeric)
        (xTy.getOrElse(?).satisfies(c), TypedExpr.Neg(typedX))
    case Chr(x: Expr) =>
        val (xTy, typedX) = check(x, Constraint.IsNumeric)
        (xTy.getOrElse(?).satisfies(c), TypedExpr.Chr(typedX))
    // Bool:
    case Not(x: Expr) =>
        val (xTy, typedX) = check(x, Constraint.IsBoolean)
        (xTy.getOrElse(?).satisfies(c), TypedExpr.Not(typedX))
    // Array of generic type:
    case Len(x: Expr) =>
        val (xTy, typedX) = check(x, Constraint.IsArray)
        (xTy.getOrElse(?).satisfies(c), TypedExpr.Len(typedX))
    // Char:
    case Ord(x: Expr) =>
        val (xTy, typedX) = check(x, Constraint.IsCharacter)
        (xTy.getOrElse(?).satisfies(c), TypedExpr.Ord(typedX))

    // TODO: ADD LITERALS
    /*
    case class IntLiteral(v: BigInt) extends Expr
    case class BoolLiteral(v: Boolean) extends Expr
    case class CharLiteral(v: Char) extends Expr
    case class StringLiteral(v: String) extends Expr
    case class Ident(v: String) extends Expr, LValue
    case class ArrayElem(v: String, indicies: List[Expr]) extends Expr, LValue
    */
}

class TypeCheckerCtx[C](tyInfo: TypeInfo, errs: mutable.Builder[Error, C]) {
    def errors: C = errs.result()

    // This will get the type of variables
    def typeOf(id: String): KnownType = tyInfo.varTys(id)

    def error(err: Error) = {
        errs += err
        None
    }
}

// This will get the most specific type out of 2 given
// This means if we know something is an Int, we can type check using that rather than ?
def mostSpecific(ty1: Option[SemType], ty2: Option[SemType]): SemType = (ty1, ty2) match {
    case (Some(?), Some(t)) => t
    case (Some(t), _)       => t
    case (None, t)          => t.getOrElse(?)
}

class TypeInfo(
    var varTys: Map[String, KnownType],
    var funcTys: Map[String, KnownType, List[KnownType]] // Check with Aidan to see how this would work
)

enum Error {
    case TypeMismatch(actual: SemType, expected: SemType)
    case NonExitableType(actual: SemType)
    case NonFreeableType(actual: SemType)
    case NonNumericType(actual: SemType)
    case NonCharacterType(actual: SemType)
    case NonBooleanType(actual: SemType)
    case NonStringType(actual: SemType)
}

enum Constraint {
    case Is(refTy: SemType)
    case IsNumeric
    case IsCharacter
    case IsBoolean
    case IsExitable
    case IsFreeable
    case IsString
}

object Constraint {
    val Unconstrained = Is(?) // Always passes
    val IsArray = Is(KnownType.Array(?))
}