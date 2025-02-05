package wacc.semantic

import wacc.ast.* 
import collection.mutable
import scala.annotation.targetName

case class TypedProg(funcs: List[TypedFunc], body: List[TypedStmt])
case class TypedFunc(t: SemType, id: TypedExpr.Ident, args: List[TypedParam], body: List[TypedStmt])
case class TypedParam(t: SemType, id: TypedExpr.Ident)

def typeCheck(prog: Prog, tyInfo: TypeInfo): Either[List[Error], TypedProg] = {
    // Note this List[Error] is non-empty. NonEmptyList import won't work
    // We will just return Right if there is no error hence avoids returning Left with an empty list!
    given ctx: TypeCheckerCtx[List[Error]] = TypeCheckerCtx(tyInfo, List.newBuilder)

    val progFuncs: List[Func] = prog.funcs
    val progStmts: List[Stmt] = prog.body

    val typedProgFuncs: List[TypedFunc] = ???
    val typedProgStmts: List[TypedStmt] = progStmts.map(check)

    ctx.errors.match {
        case err :: errs => Left(err :: errs)
        case Nil         => Right(TypedProg(typedProgFuncs, typedProgStmts))
    }
}

def check(stmt: Stmt)(using ctx: TypeCheckerCtx[?]): TypedStmt = stmt match {
    case Decl(t: Type, id: Ident, r: RValue) =>
        // This will check the type of r compared to given type t
        val (_, typedR) = check(r, Constraint.Is(ctx.typeOf(id.v)))
        TypedStmt.Decl(t, TypedExpr.Ident(id.v), typedR)
    case Asgn(l: LValue, r: RValue) =>
        // Get the type of left value
        val (ty, typedL) = check(l, Constraint.Unconstrained)
        // We need the type of the right value to match this
        val (_, typedR) = check(r, Constraint.Is(ty.getOrElse(?)))
        TypedStmt.Asgn(typedL, typedR)
    case Read(l: LValue) =>
        // Only need to  verify the LValue is actually LValue - no constraint needed? Or create constraint for IsLValue?
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
        TypedStmt.Return(typedX) // THESE NEED CHANGING
    case Print(x: Expr) =>
        val (ty, typedX) = check(x, Constraint.Unconstrained) // Create a constraint for print values!
        TypedStmt.Return(typedX) // THESE NEED CHANGING
    case Println(x: Expr) =>
        val (ty, typedX) = check(x, Constraint.Unconstrained) // Create a constraint for println values!
        TypedStmt.Return(typedX) // THESE NEED CHANGING
    case If(cond: Expr, body: List[Stmt], el: List[Stmt]) =>
        val (condTy, typedCond) = check(cond, Constraint.Unconstrained) // Create a constraint for this being a boolean!
        val typedBody = check(body, Constraint.Unconstrained) // Think this can remain as Unconstrained
        val typedEl = check(el, Constraint.Unconstrained) // As above
        TypedStmt.If(typedCond, typedBody, typedEl)
    case While(cond: Expr, body: List[Stmt]) =>
        val (condTy, typedCond) = check(cond, Constraint.Unconstrained) // Create a constraint for this being a boolean!
        val typedBody = check(body, Constraint.Unconstrained) // Think this can remain as Unconstrained
        TypedStmt.While(typedCond, typedBody)
    case CodeBlock(body: List[Stmt]) =>
        val typedBody = check(body, Constraint.Unconstrained) // Don't see why this should be anything other than Unconstrained
        TypedStmt.CodeBlock(typedBody)
    case Skip => TypedStmt.Skip()
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

    case IntLiteral(v: BigInt) => (KnownType.Int.satisfies(c), TypedExpr.IntLiteral(v))
    case BoolLiteral(v: Boolean) => (KnownType.Boolean.satisfies(c), TypedExpr.BoolLiteral(v))
    case CharLiteral(v: Char) => (KnownType.Char.satisfies(c), TypedExpr.CharLiteral(v))
    case StringLiteral(v: String) => (KnownType.String.satisfies(c), TypedExpr.StringLiteral(v))
    case Ident(v: String) => (KnownType.Ident.satisfies(c), TypedExpr.Ident(v))
    case ArrayElem(_, _) => ???
    case PairNullLiteral => ???
    case PairElem(_, _) => ???
}

def checkArithmeticExpr(x: Expr, y: Expr, c: Constraint)
                      (build: ((TypedExpr, TypedExpr) => TypedExpr))
                      (using TypeCheckerCtx[?]): (Option[SemType], TypedExpr) =
    val (xTy, typedX) = check(x, Constraint.IsNumeric)
    val (yTy, typedY) = check(y, Constraint.Is(xTy.getOrElse(?)))
    val ty = mostSpecific(xTy, yTy)
    (ty.satisfies(c), build(typedX, typedY))

def checkComparisonExpr(x: Expr, y: Expr, c: Constraint)
                       (build: ((TypedExpr, TypedExpr) => TypedExpr))
                       (using TypeCheckerCtx[?]): (Option[SemType], TypedExpr) =
    val (xTy, typedX) = check(x, Constraint.IsNumeric)
        if (xTy.getOrElse(?) == ?) {
            // X is not an int - is it a character?
            val (xTy, typedX) = check(x, Constraint.IsCharacter)
            val (yTy, typedY) = check(y, Constraint.Is(xTy.getOrElse(?)))
            val ty = mostSpecific(xTy, yTy)
            (ty.satisfies(c), TypedExpr.GreaterThan(typedX, typedY))
        } else {
            // X is an int
            val (yTy, typedY) = check(y, Constraint.Is(xTy.getOrElse(?)))
            val ty = mostSpecific(xTy, yTy)
            (ty.satisfies(c), TypedExpr.GreaterThan(typedX, typedY))
        }

def checkBooleanExpr(x: Expr, y: Expr, c: Constraint)
                      (build: ((TypedExpr, TypedExpr) => TypedExpr))
                      (using TypeCheckerCtx[?]): (Option[SemType], TypedExpr) =
    val (xTy, typedX) = check(x, Constraint.IsBoolean)
    val (yTy, typedY) = check(y, Constraint.Is(xTy.getOrElse(?)))
    val ty = mostSpecific(xTy, yTy)
    (ty.satisfies(c), build(typedX, typedY))

def check(l: LValue, c: Constraint)(using TypeCheckerCtx[?]): (Option[SemType], TypedLValue) = l match {
    case Ident(v: String) => ???
    case PairElem(index: PairIndex, v: LValue) => ???
    case ArrayElem(v: String, indices: List[Expr]) => ???
}

def check(r: RValue, c: Constraint)(using TypeCheckerCtx[?]): (Option[SemType], TypedRValue) = r match {
    case FuncCall(v: String, args: List[Expr]) => ???
        // Check if RValue needs anything special! Own constraint?
    case ArrayLiteral(xs: List[Expr]) => ???
    case PairElem(index: PairIndex, v: LValue) => ???
    case NewPair(x: Expr, y: Expr) =>
        val (xTy, typedX) = check(x, Constraint.Unconstrained)
        val (yTy, typedY) = check(y, Constraint.Is(xTy.getOrElse(?)))
        val ty = mostSpecific(xTy, yTy)
        (ty.satisfies(c), TypedRValue.NewPair(typedX, typedY))
        
    case e: Expr => check(e, c)
}

def checkReturn(t: Type, stmt: TypedStmt)(using TypeCheckerCtx[?]): Option[SemType] = (stmt, t) match {
    case (TypedStmt.Return(x: Expr), BaseType.Int) => (check(x, Constraint.Is(KnownType.Int)))._1
    case (TypedStmt.Return(x: Expr), BaseType.Bool) => (check(x, Constraint.Is(KnownType.Boolean)))._1
    case (TypedStmt.Return(x: Expr), BaseType.Char) => (check(x, Constraint.Is(KnownType.Char)))._1
    case (TypedStmt.Return(x: Expr), BaseType.String) => (check(x, Constraint.Is(KnownType.String)))._1
    case (TypedStmt.Return(x: Expr), ArrayType(_)) => (check(x, Constraint.IsArray))._1
    case (TypedStmt.Return(x: Expr), PairType(_, _)) => (check(x, Constraint.IsPair))._1
    case (TypedStmt.If(cond: Expr, body: List[TypedStmt], el: List[Stmt]), t) => 
        Some(mostSpecific(checkReturn(t, body.last), checkReturn(t, el.last)))
}

// Func(t: Type, v: String, args: List[Param], body: List[Stmt])
def check(func: Func, c: Constraint)(using ctx: TypeCheckerCtx[?]): (Option[SemType], TypedFunc) = {
    val typedParams: List[TypedParam] = func.args.map(check)
    
    val typedBody: List[TypedStmt] = func.body.map(check)

    val lastStmt: TypedStmt = typedBody.last

    val checked = checkReturn(func.t, lastStmt)

    (checked, TypedFunc(checked.getOrElse(?), TypedExpr.Ident(func.v), typedParams, typedBody))
}

def check(param: Param)(using TypeCheckerCtx[?]): TypedParam = param.t match {
    case BaseType.Int => TypedParam(KnownType.Int, TypedExpr.Ident(param.v))
    case BaseType.Bool => TypedParam(KnownType.Boolean, TypedExpr.Ident(param.v))
    case BaseType.Char => TypedParam(KnownType.Char, TypedExpr.Ident(param.v))
    case BaseType.String => TypedParam(KnownType.String, TypedExpr.Ident(param.v))
    case ArrayType(_) => TypedParam(KnownType.Array(?), TypedExpr.Ident(param.v))
    case PairType(_, _) => TypedParam(KnownType.Pair(?, ?), TypedExpr.Ident(param.v)) // check these types?
    case ErasedPairType => TypedParam(KnownType.ErasedPairType, TypedExpr.Ident(param.v))
}

@targetName("checkStmts")
def check(stmts: List[Stmt], c: Constraint)(using TypeCheckerCtx[?]): List[TypedStmt] = stmts.map(check(_))

@targetName("checkExprs")
def check(listArgs: List[Expr], c: Constraint)(using TypeCheckerCtx[?]): (Option[SemType], List[TypedExpr]) = {
    val mappedArgs: List[(Option[SemType], TypedExpr)] = listArgs.map(check(_, c))
    val semTypes: List[Option[SemType]] = mappedArgs.map(_._1)
    val typedExprs: List[TypedExpr] = mappedArgs.map(_._2)

    val ty: SemType = semTypes.fold(Some(?))((t1, t2) => Some(mostSpecific(t1, t2))).getOrElse(?)

    (ty.satisfies(c), typedExprs)
}

@targetName("checkFuncs")
def check(funcs: List[Func], c: Constraint)(using TypeCheckerCtx[?]): List[TypedFunc] = {
    ???
}

// This will get the most specific type out of 2 given
// This means if we know something is an Int, we can type check using that rather than ?
def mostSpecific(ty1: Option[SemType], ty2: Option[SemType]): SemType = (ty1, ty2) match {
    case (Some(?), Some(t)) => t
    case (Some(t), _)       => t
    case (None, t)          => t.getOrElse(?)
}

extension (ty: SemType) def ~(refTy: SemType): Option[SemType] = (ty, refTy) match
    case (?, refTy) => ty ~ refTy
    case (ty, ?) => Some(ty)
    case (ty, refTy) if ty == refTy => Some(ty)
    // case (ArrayLiteral(typ), ArrayLiteral(refTyp)) => ty ~ refTy
    case _ => None

extension (ty: SemType) def satisfies (c: Constraint)(using ctx: TypeCheckerCtx[?]): Option[SemType] = (ty, c) match {
    case (ty, Constraint.Is(refTy)) => (ty ~ refTy).orElse {
        ctx.error(Error.TypeMismatch(ty, refTy))
    }
    case (?, _) => Some(?)
    case (kty@KnownType.Int, Constraint.IsNumeric) => Some(kty)
    case (kty, Constraint.IsNumeric) => ctx.error(Error.NonNumericType(kty))
    case (kty@KnownType.Char, Constraint.IsCharacter) => Some(kty)
    case (kty, Constraint.IsCharacter) => ctx.error(Error.NonCharacterType(kty))
    case (kty@KnownType.Boolean, Constraint.IsBoolean) => Some(kty)
    case (kty, Constraint.IsBoolean) => ctx.error(Error.NonCharacterType(kty))
    case (kty@KnownType.String, Constraint.IsString) => Some(kty)
    case (kty, Constraint.IsString) => ctx.error(Error.NonStringType(kty))
    case (kty@KnownType.Int, Constraint.IsExitable) => Some(kty)
    case (kty, Constraint.IsExitable) => ctx.error(Error.NonExitableType(kty))
    case (kty@(KnownType.Array(_) | KnownType.Pair(_, _)), Constraint.IsFreeable) => Some(kty)
    case (kty, Constraint.IsFreeable) => ctx.error(Error.NonFreeableType(kty))
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

// The typer will take this in
// It maps variable names & function names to their types
// This should allow for variables and functions to share names
class TypeInfo(
    var varTys: Map[String, KnownType],
    var funcTys: Map[(String, KnownType), Map[String, KnownType]]
    // This is a map from (Function Identifier, Return Type) -> parameter map [Param name -> Param type]
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
    val IsPair = Is(KnownType.Pair(?, ?))
}