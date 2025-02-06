package wacc.semantic

import wacc.*
import wacc.ast.*
import wacc.q_ast.*
import collection.mutable
import scala.annotation.targetName
import wacc.KnownType.Pair

case class TypedProg(funcs: List[TypedFunc], body: List[TypedStmt])
case class TypedFunc(t: SemType, id: TypedExpr.Ident, args: List[TypedParam], body: List[TypedStmt])
case class TypedParam(t: SemType, id: TypedExpr.Ident)

def typeCheck(prog: Q_Prog, tyInfo: TypeInfo): Either[List[Error], TypedProg] = {
    // Note this List[Error] is non-empty. NonEmptyList import won't work
    // We will just return Right if there is no error hence avoids returning Left with an empty list!
    given ctx: TypeCheckerCtx[List[Error]] = TypeCheckerCtx(tyInfo, List.newBuilder)

    val progFuncs: List[Q_Func] = prog.funcs
    val progStmts: List[Q_Stmt] = prog.body

    val typedProgFuncs: List[TypedFunc] = progFuncs.map(f => check(f, Constraint.Unconstrained)._2)
    val typedProgStmts: List[TypedStmt] = progStmts.map(check)

    ctx.errors.match {
        case err :: errs => Left(err :: errs)
        case Nil         => Right(TypedProg(typedProgFuncs, typedProgStmts))
    }
}

def check(stmt: Q_Stmt)(using ctx: TypeCheckerCtx[?]): TypedStmt = stmt match {
    case Q_Decl(id: Q_Name, r: Q_RValue) =>
        // This will check the type of r compared to given type t
        val (_, typedR) = check(r, Constraint.Is(ctx.typeOf(id)))
        TypedStmt.Decl(TypedExpr.Ident(id), typedR)
    case Q_Asgn(l: Q_LValue, r: Q_RValue) =>
        // Get the type of left value
        val (ty, typedL) = check(l, Constraint.Unconstrained)
        // We need the type of the right value to match this
        val (_, typedR) = check(r, Constraint.Is(ty.getOrElse(?)))
        TypedStmt.Asgn(typedL, typedR)
    case Q_Read(l: Q_LValue) =>
        // Only need to  verify the LValue is actually LValue - no constraint needed? Or create constraint for IsLValue?
        val (ty, typedL) = check(l, Constraint.IsReadable)
        TypedStmt.Read(typedL)
    case Q_Free(x: Q_Expr) =>
        val (ty, typedX) = check(x, Constraint.IsFreeable) // ADD THE CONSTRANTS FOR FREEABLE, EXITABLE
        TypedStmt.Free(typedX)
    case Q_Return(x: Q_Expr) =>
        val (ty, typedX) = check(x, Constraint.Unconstrained)
        TypedStmt.Return(typedX)
    case Q_Exit(x: Q_Expr) =>
        val (ty, typedX) = check(x, Constraint.IsExitable)
        TypedStmt.Exit(typedX)
    case Q_Print(x: Q_Expr) =>
        val (ty, typedX) = check(x, Constraint.Unconstrained)
        TypedStmt.Print(typedX)
    case Q_Println(x: Q_Expr) =>
        val (ty, typedX) = check(x, Constraint.Unconstrained)
        TypedStmt.Println(typedX)
    case Q_If(cond: Q_Expr, body: List[Q_Stmt], scopedBody: Set[Q_Name], el: List[Q_Stmt], scopedEl: Set[Q_Name]) =>
        val (condTy, typedCond) = check(cond, Constraint.Unconstrained) // Create a constraint for this being a boolean!
        val typedBody = check(body, Constraint.Unconstrained) // Think this can remain as Unconstrained
        val typedEl = check(el, Constraint.Unconstrained) // As above
        TypedStmt.If(typedCond, typedBody, typedEl)
    case Q_While(cond: Q_Expr, body: List[Q_Stmt], scopedBody: Set[Q_Name]) =>
        val (condTy, typedCond) = check(cond, Constraint.Unconstrained) // Create a constraint for this being a boolean!
        val typedBody = check(body, Constraint.Unconstrained) // Think this can remain as Unconstrained
        TypedStmt.While(typedCond, typedBody)
    case Q_CodeBlock(body: List[Q_Stmt], scopedBody: Set[Q_Name]) =>
        val typedBody = check(body, Constraint.Unconstrained) // Don't see why this should be anything other than Unconstrained
        TypedStmt.CodeBlock(typedBody)
    case Q_Skip => TSkip
}

def check(expr: Q_Expr, c: Constraint)(using TypeCheckerCtx[?]): (Option[SemType], TypedExpr) = expr match {
    // The below only works on two ints
    case Q_Mul(x: Q_Expr, y: Q_Expr) => checkArithmeticExpr(x, y, c)(TypedExpr.Mul.apply)
    case Q_Div(x: Q_Expr, y: Q_Expr) => checkArithmeticExpr(x, y, c)(TypedExpr.Div.apply)
    case Q_Mod(x: Q_Expr, y: Q_Expr) => checkArithmeticExpr(x, y, c)(TypedExpr.Mod.apply)
    case Q_Add(x: Q_Expr, y: Q_Expr) => checkArithmeticExpr(x, y, c)(TypedExpr.Add.apply)
    case Q_Sub(x: Q_Expr, y: Q_Expr) => checkArithmeticExpr(x, y, c)(TypedExpr.Div.apply)

    // The below work on two ints or two chars
    case Q_GreaterThan(x: Q_Expr, y: Q_Expr) => checkComparisonExpr(x, y, c)(TypedExpr.GreaterThan.apply)
    case Q_GreaterThanEq(x: Q_Expr, y: Q_Expr) => checkComparisonExpr(x, y, c)(TypedExpr.GreaterThanEq.apply)
    case Q_LessThan(x: Q_Expr, y: Q_Expr) => checkComparisonExpr(x, y, c)(TypedExpr.LessThan.apply)
    case Q_LessThanEq(x: Q_Expr, y: Q_Expr) => checkComparisonExpr(x, y, c)(TypedExpr.LessThanEq.apply)

    // The below work on two of the same type
    case Q_Eq(x: Q_Expr, y: Q_Expr) =>
        val (xTy, typedX) = check(x, Constraint.Is(?))
        val (yTy, typedY) = check(y, Constraint.Is(xTy.getOrElse(?)))
        val ty = mostSpecific(xTy, yTy)
        (ty.satisfies(c), TypedExpr.Eq(typedX, typedY))
    case Q_NotEq(x: Q_Expr, y: Q_Expr) => 
        val (xTy, typedX) = check(x, Constraint.Is(?))
        val (yTy, typedY) = check(y, Constraint.Is(xTy.getOrElse(?)))
        val ty = mostSpecific(xTy, yTy)
        (ty.satisfies(c), TypedExpr.NotEq(typedX, typedY))

    // The below only work on two bools
    case Q_And(x: Q_Expr, y: Q_Expr) => checkBooleanExpr(x, y, c)(TypedExpr.And.apply)
    case Q_Or(x: Q_Expr, y: Q_Expr) => checkBooleanExpr(x, y, c)(TypedExpr.Or.apply)

    // The below are unary
    // Int:
    case Q_Neg(x: Q_Expr) =>
        val (xTy, typedX) = check(x, Constraint.IsNumeric)
        (xTy.getOrElse(?).satisfies(c), TypedExpr.Neg(typedX))
    case Q_Chr(x: Q_Expr) =>
        val (xTy, typedX) = check(x, Constraint.IsNumeric)
        (xTy.getOrElse(?).satisfies(c), TypedExpr.Chr(typedX))
    // Bool:
    case Q_Not(x: Q_Expr) =>
        val (xTy, typedX) = check(x, Constraint.IsBoolean)
        (xTy.getOrElse(?).satisfies(c), TypedExpr.Not(typedX))
    // Array of generic type:
    case Q_Len(x: Q_Expr) =>
        val (xTy, typedX) = check(x, Constraint.IsArray)
        (xTy.getOrElse(?).satisfies(c), TypedExpr.Len(typedX))
    // Char:
    case Q_Ord(x: Q_Expr) =>
        val (xTy, typedX) = check(x, Constraint.IsCharacter)
        (xTy.getOrElse(?).satisfies(c), TypedExpr.Ord(typedX))

    case Q_IntLiteral(v: BigInt) => (KnownType.Int.satisfies(c), TypedExpr.IntLiteral(v))
    case Q_BoolLiteral(v: Boolean) => (KnownType.Boolean.satisfies(c), TypedExpr.BoolLiteral(v))
    case Q_CharLiteral(v: Char) => (KnownType.Char.satisfies(c), TypedExpr.CharLiteral(v))
    case Q_StringLiteral(v: String) => (KnownType.String.satisfies(c), TypedExpr.StringLiteral(v))
    case Q_Ident(v: Q_Name) => (KnownType.Ident.satisfies(c), TypedExpr.Ident(v))
    case Q_ArrayElem(v: Q_Name, indices: List[Q_Expr]) => 
        val checkedExprs: List[(Option[SemType], TypedExpr)] = indices.map(expr => check(expr, Constraint.IsNumeric))
        val semTypes = checkedExprs.map(_._1)
        val typedExprs = checkedExprs.map(_._2)

        val ty = semTypes.fold(Some(?))((t1, t2) => Some(mostSpecific(t1, t2))).getOrElse(?)

        (ty.satisfies(c), TypedExpr.ArrayElem(TypedExpr.Ident(v), typedExprs))
    case Q_PairNullLiteral => (KnownType.Pair(?, ?).satisfies(c), TPairNullLiteral)
    case Q_PairElem(index: PairIndex, v: Q_LValue) => 
        val (vTy, typedV) = check(v, c)
        (vTy.getOrElse(?).satisfies(c), TypedRValue.PairElem(index, typedV))
        
}

def checkArithmeticExpr(x: Q_Expr, y: Q_Expr, c: Constraint)
                      (build: ((TypedExpr, TypedExpr) => TypedExpr))
                      (using TypeCheckerCtx[?]): (Option[SemType], TypedExpr) =
    val (xTy, typedX) = check(x, Constraint.IsNumeric)
    val (yTy, typedY) = check(y, Constraint.Is(xTy.getOrElse(?)))
    val ty = mostSpecific(xTy, yTy)
    (ty.satisfies(c), build(typedX, typedY))

def checkComparisonExpr(x: Q_Expr, y: Q_Expr, c: Constraint)
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

def checkBooleanExpr(x: Q_Expr, y: Q_Expr, c: Constraint)
                      (build: ((TypedExpr, TypedExpr) => TypedExpr))
                      (using TypeCheckerCtx[?]): (Option[SemType], TypedExpr) =
    val (xTy, typedX) = check(x, Constraint.IsBoolean)
    val (yTy, typedY) = check(y, Constraint.Is(xTy.getOrElse(?)))
    val ty = mostSpecific(xTy, yTy)
    (ty.satisfies(c), build(typedX, typedY))

def check(l: Q_LValue, c: Constraint)(using ctx: TypeCheckerCtx[?]): (Option[SemType], TypedLValue) = l match {
    case Q_Ident(v: Q_Name) => (ctx.typeOf(v).satisfies(c), TypedExpr.Ident(v))
    case Q_PairElem(index: PairIndex, v: Q_LValue) =>
        val (vTy, typedV) = check(v, c)
        (vTy.getOrElse(?).satisfies(c), TypedRValue.PairElem(index, typedV))
    case Q_ArrayElem(v: Q_Name, indices: List[Q_Expr]) =>
        val checkedExprs: List[(Option[SemType], TypedExpr)] = indices.map(expr => check(expr, Constraint.IsNumeric))
        val semTypes = checkedExprs.map(_._1)
        val typedExprs = checkedExprs.map(_._2)

        val ty = semTypes.fold(Some(?))((t1, t2) => Some(mostSpecific(t1, t2))).getOrElse(?)

        (ty.satisfies(c), TypedExpr.ArrayElem(TypedExpr.Ident(v), typedExprs))
}

def check(r: Q_RValue, c: Constraint)(using ctx: TypeCheckerCtx[?]): (Option[SemType], TypedRValue) = r match {
    case Q_FuncCall(v: Q_Name, args: List[Q_Expr]) =>
        val (exprTy, typedExprs): (Option[SemType], List[TypedExpr]) = check(args, c)

        (exprTy, TypedRValue.FuncCall(v.name, typedExprs))
    case Q_ArrayLiteral(xs: List[Q_Expr]) => 
        val checkedExprs: List[(Option[SemType], TypedExpr)] = xs.map(x => check(x, Constraint.Unconstrained))
        val semTypes = checkedExprs.map(_._1)
        val typedExprs = checkedExprs.map(_._2)

        val ty: SemType = semTypes.fold(Some(?))((t1, t2) => Some(mostSpecific(t1, t2))).getOrElse(?)

        (KnownType.Array(ty).satisfies(c), TypedRValue.ArrayLiteral(typedExprs, ty))
    case Q_PairElem(index: PairIndex, v: Q_LValue) => 
        val (vTy, typedV) = check(v, c)
        (vTy.getOrElse(?).satisfies(c), TypedRValue.PairElem(index, typedV))
    case Q_NewPair(x: Q_Expr, y: Q_Expr) =>
        val (xTy, typedX) = check(x, Constraint.Unconstrained)
        val (yTy, typedY) = check(y, Constraint.Is(xTy.getOrElse(?)))
        val ty = mostSpecific(xTy, yTy)
        (ty.satisfies(c), TypedRValue.NewPair(typedX, typedY))
        
    case e: Q_Expr => check(e, c)
}

def checkReturn(t: Type, stmt: TypedStmt)(using ctx: TypeCheckerCtx[?]): Option[SemType] = (stmt, t) match {
    case (TypedStmt.Return(x: Q_Expr), ty) => check(x, Constraint.Is(toSemType(ty)))._1
    case (TypedStmt.Exit(x: Q_Expr), _) => check(x, Constraint.Is(KnownType.Int))._1
    case (TypedStmt.If(cond: Q_Expr, body: List[TypedStmt], el: List[TypedStmt]), t) => 
        Some(mostSpecific(checkReturn(t, body.last), checkReturn(t, el.last)))
    case (TypedStmt.CodeBlock(stmts: List[TypedStmt]), t) => checkReturn(t, stmts.last)
    case (_, _) => throw SyntaxFailureException("Last statement is not a return/if. This should be dealt with in parsing")
}

// Func(t: Type, v: String, args: List[Param], body: List[Stmt])
def check(func: Q_Func, c: Constraint)(using ctx: TypeCheckerCtx[?]): (Option[SemType], TypedFunc) = {
    val typedParams: List[TypedParam] = func.args.map(check)
    
    val typedBody: List[TypedStmt] = func.body.map(check)

    val lastStmt: TypedStmt = typedBody.last

    val checked = checkReturn(func.t, lastStmt)

    (checked, TypedFunc(checked.getOrElse(?), TypedExpr.Ident(func.v), typedParams, typedBody))
}

def check(param: Q_Param)(using TypeCheckerCtx[?]): TypedParam = param.t match {
    case BaseType.Int => TypedParam(KnownType.Int, TypedExpr.Ident(param.v))
    case BaseType.Bool => TypedParam(KnownType.Boolean, TypedExpr.Ident(param.v))
    case BaseType.Char => TypedParam(KnownType.Char, TypedExpr.Ident(param.v))
    case BaseType.String => TypedParam(KnownType.String, TypedExpr.Ident(param.v))
    case ArrayType(t: Type) => TypedParam(KnownType.Array(toSemType(t)), TypedExpr.Ident(param.v))
    case PairType(t1: Type, t2: Type) => TypedParam(KnownType.Pair(toSemType(t1), toSemType(t2)), TypedExpr.Ident(param.v)) 
    case ErasedPairType => TypedParam(KnownType.Pair(?, ?), TypedExpr.Ident(param.v))
}

@targetName("checkStmts")
def check(stmts: List[Q_Stmt], c: Constraint)(using TypeCheckerCtx[?]): List[TypedStmt] = stmts.map(check(_))

@targetName("checkExprs")
def check(listArgs: List[Q_Expr], c: Constraint)(using TypeCheckerCtx[?]): (Option[SemType], List[TypedExpr]) = {
    val mappedArgs: List[(Option[SemType], TypedExpr)] = listArgs.map(check(_, c))
    val semTypes: List[Option[SemType]] = mappedArgs.map(_._1)
    val typedExprs: List[TypedExpr] = mappedArgs.map(_._2)

    val ty: SemType = semTypes.fold(Some(?))((t1, t2) => Some(mostSpecific(t1, t2))).getOrElse(?)

    (ty.satisfies(c), typedExprs)
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
    case (kty@(KnownType.Int | KnownType.Char), Constraint.IsReadable) => Some(kty)
    case (kty, Constraint.IsReadable) => ctx.error(Error.NonReadableType(kty))
}

class TypeCheckerCtx[C](tyInfo: TypeInfo, errs: mutable.Builder[Error, C]) {
    def errors: C = errs.result()

    // This will get the type of variables
    def typeOf(id: Q_Name): KnownType = tyInfo.varTys(id)
    def typeOfFunc(id: Q_Name): (KnownType, List[Q_Name]) = tyInfo.funcTys(id)

    def error(err: Error) = {
        errs += err
        None
    }
}

enum Error {
    case TypeMismatch(actual: SemType, expected: SemType)
    case NonExitableType(actual: SemType)
    case NonFreeableType(actual: SemType)
    case NonNumericType(actual: SemType)
    case NonCharacterType(actual: SemType)
    case NonBooleanType(actual: SemType)
    case NonStringType(actual: SemType)
    case NonReadableType(actual: SemType)
}

enum Constraint {
    case Is(refTy: SemType)
    case IsNumeric
    case IsCharacter
    case IsBoolean
    case IsExitable
    case IsFreeable
    case IsString
    case IsReadable
}

object Constraint {
    val Unconstrained = Is(?) // Always passes
    val IsArray = Is(KnownType.Array(?))
    val IsPair = Is(KnownType.Pair(?, ?))
}