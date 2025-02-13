package wacc.semantic

import wacc.*
import wacc.ast.*
import wacc.q_ast.*
import collection.mutable
import scala.annotation.targetName
import wacc.KnownType.Pair

def typeCheck(prog: Q_Prog, tyInfo: TypeInfo, fname: Option[String] = None): Option[List[Err]] = {
    // Note this List[Error] is non-empty. NonEmptyList import won't work
    // We will just return Right if there is no error hence avoids returning Left with an empty list!
    given ctx: TypeCheckerCtx = TypeCheckerCtx(tyInfo, mutable.ListBuffer(), fname)

    val progFuncs: List[Q_Func] = prog.funcs
    val progStmts: List[Q_Stmt] = prog.body
    
    progFuncs.map(check(_, Constraint.Unconstrained))
    progStmts.map(check(_, isFunc = false, Constraint.Unconstrained))

    ctx.errors.match {
        case err :: errs => Some(err :: errs)
        case Nil         => None
    }
}

// in the case where l = Array, r = ident => get r from typeInfo, check l IsExactly r (==)

// l != array, check l against r => l == r

// break into inner of array, map function on each inner member of literal on right

def checkDeclTypes(l: SemType, r: Q_RValue)(using ctx: TypeCheckerCtx): Option[SemType] = {
    (l, r) match {
        // this breaks into inner of array then maps
        case (KnownType.Array(arrT@KnownType.Array(_)), Q_ArrayLiteral(xs: List[Q_Expr], pos)) => 
            // l = array, r = array literal
            // so check each member of r is arrT
            // we can do this by calling checkArrayTypes with arrT and each member type of r
            ctx.setPos(pos)
            val opts = xs.map(checkDeclTypes(arrT, _))

            val semOpt: Option[SemType] = opts.fold(Some(?))(_.getOrElse(?) ~ _.getOrElse(?)) // ? could get changed for arrT potentially!

            semOpt match {
                case Some(?) => None // most generic type is ? hence we don't have any common type within the right array?
                case _ => semOpt
            }
        case (t@KnownType.Array(arrT), Q_ArrayLiteral(xs, pos)) =>
            ctx.setPos(pos)
            val semOpt: SemType = 
                if xs.isEmpty then
                    arrT
                else
                    xs
                        .map(check(_, Constraint.Unconstrained))
                        .fold(Some(?))((t1, t2) => t1.getOrElse(?).satisfies(Constraint.Is(t2.getOrElse(?)))).getOrElse(?)
            arrT match
                case KnownType.String =>
                    if semOpt == KnownType.String | semOpt == KnownType.Array(KnownType.Char) then
                        Some(t)
                    else
                        ctx.error(TypeMismatch(KnownType.Array(semOpt), t))
                case _ =>
                    if semOpt == arrT then
                        Some(t)
                    else
                        ctx.error(TypeMismatch(KnownType.Array(semOpt), t))
        case (_, Q_Ident(n, pos)) => 
            ctx.setPos(pos)
            val t = ctx.typeOf(n)
            (l, t) match
                case (KnownType.String, KnownType.Array(KnownType.Char)) =>
                    Some(l)
                case _ =>
                    if l == t then
                        Some(l)
                    else
                        ctx.error(TypeMismatch(t, l))
            // check(r, Constraint.IsExactly(l))
        // catches the base value case and the case where r is an ident
        case (_, _) => 
            check(r, Constraint.Is(l))
    }
}

def foo(l: SemType, t: SemType): Boolean = (l, t) match
    case (KnownType.String, KnownType.Array(KnownType.Char)) =>
        true
    case _ =>
        false


//def checkArrayTypes(l: SemType, r: SemType): Option[SemType] = (l, r) match {
//    ??? // Not sure if this part is necessary
//}

def check(stmt: Q_Stmt, isFunc: Boolean, funcConstraint: Constraint)(using ctx: TypeCheckerCtx): Unit =
    stmt match {
    case Q_Decl(id: Q_Name, r: Q_RValue, pos) =>
        ctx.setPos(pos)
        //check(r, Constraint.Is(ctx.typeOf(id))) // This will check the type of r compared to given type t
        checkDeclTypes(ctx.typeOf(id), r)
    // Check the type of the LValue matches that of the RValue
    case Q_Asgn(l: Q_LValue, r: Q_RValue, pos) => 
        ctx.setPos(pos)
        check(r, Constraint.Is(check(l, Constraint.Unconstrained).getOrElse(?)))
    // Only need to  verify the LValue is actually LValue - no constraint needed? Or create constraint for IsLValue?
    case Q_Read(l: Q_LValue, pos) => 
        ctx.setPos(pos)
        check(l, Constraint.IsReadable) 
    case Q_Free(x: Q_Expr, pos) => 
        ctx.setPos(pos)
        check(x, Constraint.IsFreeable) // ADD THE CONSTRANTS FOR FREEABLE, EXITABLE
    case Q_Return(x: Q_Expr, pos) => 
        ctx.setPos(pos)
        if isFunc then 
            check(x, funcConstraint)
        else
            ctx.error(InvalidReturn())
    case Q_Exit(x: Q_Expr, pos) => 
        ctx.setPos(pos)
        check(x, Constraint.IsExitable)
    case Q_Print(x: Q_Expr, pos) => 
        ctx.setPos(pos)
        check(x, Constraint.Unconstrained)
    case Q_Println(x: Q_Expr, pos) => 
        ctx.setPos(pos)
        check(x, Constraint.Unconstrained)
    case Q_If(cond: Q_Expr, body: List[Q_Stmt], _, el: List[Q_Stmt], _, pos) =>
        ctx.setPos(pos)
        check(cond, Constraint.IsBoolean)
        check(body, isFunc, funcConstraint) // Think this can remain as Unconstrained
        check(el, isFunc, funcConstraint) // As above
    case Q_While(cond: Q_Expr, body: List[Q_Stmt], scopedBody: Set[Q_Name], pos) =>
        ctx.setPos(pos)
        check(cond, Constraint.IsBoolean)
        check(body, isFunc, funcConstraint) // Think this can remain as Unconstrained
    case Q_CodeBlock(body: List[Q_Stmt], scopedBody: Set[Q_Name], pos) =>
        ctx.setPos(pos)
        check(body, isFunc, funcConstraint) // Don't see why this should be anything other than Unconstrained
    case _ => ()
}

def check(expr: Q_Expr, c: Constraint)(using ctx: TypeCheckerCtx): Option[SemType] = expr match {
    // The below only works on two ints
    case Q_Mul(x: Q_Expr, y: Q_Expr, pos) => 
        ctx.setPos(pos)
        checkArithmeticExpr(x, y, c)
    case Q_Div(x: Q_Expr, y: Q_Expr, pos) => 
        ctx.setPos(pos)
        checkArithmeticExpr(x, y, c)
    case Q_Mod(x: Q_Expr, y: Q_Expr, pos) => 
        ctx.setPos(pos)
        checkArithmeticExpr(x, y, c)
    case Q_Add(x: Q_Expr, y: Q_Expr, pos) => 
        ctx.setPos(pos)
        checkArithmeticExpr(x, y, c)
    case Q_Sub(x: Q_Expr, y: Q_Expr, pos) => 
        ctx.setPos(pos)
        checkArithmeticExpr(x, y, c)

    // The below work on two ints or two chars
    case Q_GreaterThan(x: Q_Expr, y: Q_Expr, pos) => 
        ctx.setPos(pos)
        checkComparisonExpr(x, y, c)
    case Q_GreaterThanEq(x: Q_Expr, y: Q_Expr, pos) => 
        ctx.setPos(pos)
        checkComparisonExpr(x, y, c)
    case Q_LessThan(x: Q_Expr, y: Q_Expr, pos) => 
        ctx.setPos(pos)
        checkComparisonExpr(x, y, c)
    case Q_LessThanEq(x: Q_Expr, y: Q_Expr, pos) => 
        ctx.setPos(pos)
        checkComparisonExpr(x, y, c)

    // The below work on two of the same type
    case Q_Eq(x: Q_Expr, y: Q_Expr, pos) => 
        ctx.setPos(pos)
        check(y, Constraint.Is(check(x, Constraint.Is(?)).getOrElse(?)))
    case Q_NotEq(x: Q_Expr, y: Q_Expr, pos) => 
        ctx.setPos(pos)
        check(y, Constraint.Is(check(x, Constraint.Is(?)).getOrElse(?)))

    // The below only work on two bools
    case Q_And(x: Q_Expr, y: Q_Expr, pos) => 
        ctx.setPos(pos)
        checkBooleanExpr(x, y, c)
    case Q_Or(x: Q_Expr, y: Q_Expr, pos) => 
        ctx.setPos(pos)
        checkBooleanExpr(x, y, c)

    // The below are unary
    // Int:
    case Q_Neg(x: Q_Expr, pos) =>
        ctx.setPos(pos)
        check(x, Constraint.IsNumeric).getOrElse(?).satisfies(c)
    case Q_Chr(x: Q_Expr, pos) =>
        ctx.setPos(pos)
        check(x, Constraint.IsNumeric).getOrElse(?)
        KnownType.Char.satisfies(c)
    // Bool:
    case Q_Not(x: Q_Expr, pos) =>
        ctx.setPos(pos)
        check(x, Constraint.IsBoolean).getOrElse(?).satisfies(c)
        KnownType.Boolean.satisfies(c)
    // Array of generic type:
    case Q_Len(x: Q_Expr, pos) =>
        ctx.setPos(pos)
        check(x, Constraint.IsArray).getOrElse(?)
        KnownType.Int.satisfies(c)
    // Char:
    case Q_Ord(x: Q_Expr, pos) =>
        ctx.setPos(pos)
        check(x, Constraint.IsCharacter).getOrElse(?)
        KnownType.Int.satisfies(c)

    case Q_IntLiteral(v: BigInt, pos) => 
        ctx.setPos(pos)
        KnownType.Int.satisfies(c)
    case Q_BoolLiteral(v: Boolean, pos) => 
        ctx.setPos(pos)
        KnownType.Boolean.satisfies(c)
    case Q_CharLiteral(v: Char, pos) => 
        ctx.setPos(pos)
        KnownType.Char.satisfies(c)
    case Q_StringLiteral(v: String, pos) => 
        ctx.setPos(pos)
        KnownType.String.satisfies(c)
    case Q_Ident(v: Q_Name, pos) => 
        ctx.setPos(pos)
        ctx.typeOf(v).satisfies(c)
    case Q_ArrayElem(v: Q_Name, indices: List[Q_Expr], pos) => 
        ctx.setPos(pos)
        checkArray(indices, v, c)
    case Q_PairNullLiteral => KnownType.Pair(?, ?).satisfies(c)
    case Q_PairElem(index: PairIndex, v: Q_LValue, pos) => 
        ctx.setPos(pos)
        checkPairElem(v, c)
}

def checkArray(indices: List[Q_Expr], v: Q_Name, c: Constraint)(using ctx: TypeCheckerCtx): Option[SemType] =
    indices.map(expr => check(expr, Constraint.IsNumeric))
    var t: SemType = ctx.typeOf(v) 
    for _ <- 1 to indices.length do
        t match
            case KnownType.Array(_t) => t = _t
            case _ => 
                ctx.error(InvalidIndexing())
    
    t.satisfies(c)



def checkArithmeticExpr(x: Q_Expr, y: Q_Expr, c: Constraint)
                       (using TypeCheckerCtx): Option[SemType] =
    val xTy = check(x, Constraint.IsNumeric)
    val yTy = check(y, Constraint.Is(xTy.getOrElse(?)))
    mostSpecific(xTy, yTy).satisfies(c)

def checkComparisonExpr(x: Q_Expr, y: Q_Expr, c: Constraint)
                       (using TypeCheckerCtx): Option[SemType] =
    check(x, Constraint.IsNumericOrCharacter)
    val xTy = check(x, Constraint.IsNumericNoError)
    if (xTy.getOrElse(?) == ?) {
        // X is not an int - it should be a character
        check(x, Constraint.IsCharacter)
        check(y, Constraint.IsCharacter)
    } else {
        // X is an int
        check(y, Constraint.Is(xTy.getOrElse(?)))
    }
    KnownType.Boolean.satisfies(c)

def checkBooleanExpr(x: Q_Expr, y: Q_Expr, c: Constraint)
                    (using TypeCheckerCtx): Option[SemType] =
    val xTy = check(x, Constraint.IsBoolean)
    val yTy = check(y, Constraint.Is(xTy.getOrElse(?)))
    mostSpecific(xTy, yTy).satisfies(c)

def check(l: Q_LValue, c: Constraint)(using ctx: TypeCheckerCtx): Option[SemType] = l match {
    case Q_Ident(v: Q_Name, pos) => 
        ctx.setPos(pos)
        ctx.typeOf(v).satisfies(c)
    case Q_PairElem(index: PairIndex, v: Q_LValue, pos) => 
        ctx.setPos(pos)
        checkPairElem(l, c)
    case Q_ArrayElem(v: Q_Name, indices: List[Q_Expr], pos) => 
        ctx.setPos(pos)
        checkArray(indices, v, c)
}

def checkPairElem(l: Q_LValue, c: Constraint)(using ctx: TypeCheckerCtx): Option[SemType] = l match {
    case Q_PairElem(index: PairIndex, v: Q_LValue, _) => 
        val pairType: SemType = check(v, Constraint.Is(KnownType.Pair(?, ?))).getOrElse(?)
        if pairType == KnownType.Pair(?, ?) then
            return ctx.error(UnknownType(pairType))
        val kt: KnownType.Pair = pairType.asInstanceOf[KnownType.Pair]

        index match {
            case PairIndex.First  => 
                kt.ty1.satisfies(c)
            case PairIndex.Second => 
                kt.ty2.satisfies(c)
        }
    // This case should never be reached as we call checkPairElem on pair elems only
    case _ => throw Exception("tried to use checkPairElem on something other than a Q_PairElem")
}

def check(r: Q_RValue, c: Constraint)(using ctx: TypeCheckerCtx): Option[SemType] =
    r match {
    case Q_FuncCall(v: Q_Name, args: List[Q_Expr], pos) =>
        ctx.setPos(pos)
        val returnType: KnownType = ctx.typeOfFunc(v)._1
        val argNames: List[Q_Name] = ctx.typeOfFunc(v)._2

        if (args.length != argNames.length) then
            ctx.error(WrongNumberOfArgs(args.length, argNames.length))
        else
            for (i <- 0 to args.length - 1) {
                val expectedType: KnownType = ctx.typeOf(argNames(i))
                
                check(args(i), Constraint.Is(expectedType))
            }

        returnType.satisfies(c)
    case Q_ArrayLiteral(xs: List[Q_Expr], _) =>
        val ty = xs
            .map(check(_, Constraint.Unconstrained))
            .fold(Some(?))((t1, t2) => t1.getOrElse(?).satisfies(Constraint.Is(t2.getOrElse(?)))).getOrElse(X)
        KnownType.Array(ty).satisfies(c)
    case Q_PairElem(index: PairIndex, v: Q_LValue, pos) =>
        ctx.setPos(pos)  
        val pairType: SemType = check(v, Constraint.Is(KnownType.Pair(?, ?))).getOrElse(?)
        val kt: KnownType.Pair = pairType.asInstanceOf[KnownType.Pair]

        index match {
            case PairIndex.First  => 
                kt.ty1.satisfies(c)
            case PairIndex.Second => 
                kt.ty2.satisfies(c)
        }
    case Q_NewPair(x: Q_Expr, y: Q_Expr, pos) =>
        ctx.setPos(pos)
        KnownType.Pair(check(x, Constraint.Unconstrained).getOrElse(?), check(y, Constraint.Unconstrained).getOrElse(?)).satisfies(c)
    case e: Q_Expr => check(e, c)
}

def checkReturnType(t: Type, stmt: Q_Stmt)(using ctx: TypeCheckerCtx): Option[SemType] = (stmt, t) match {
    case (Q_Return(x: Q_Expr, pos), ty) => 
        ctx.setPos(pos)
        check(x, Constraint.Is(toSemType(ty)))
    case (Q_Exit(x: Q_Expr, pos), _) => 
        ctx.setPos(pos)
        check(x, Constraint.Is(KnownType.Int))
    case (Q_If(cond: Q_Expr, body: List[Q_Stmt], _, el: List[Q_Stmt], _, pos), t) =>
        ctx.setPos(pos)
        Some(mostSpecific(checkReturnType(t, body.last), checkReturnType(t, el.last)))
    case (Q_CodeBlock(stmts: List[Q_Stmt], _, pos), t) => 
        ctx.setPos(pos)
        checkReturnType(t, stmts.last)
    case (_, _) => throw SyntaxFailureException("Last statement is not a return/if. This should be dealt with in parsing")
}

def check(func: Q_Func, c: Constraint)(using ctx: TypeCheckerCtx): Option[SemType] = {
    func.body.map(check(_, isFunc = true, Constraint.Is(toSemType(func.t))))
    checkReturnType(func.t, func.body.last)
}

@targetName("checkStmts")
def check(stmts: List[Q_Stmt], isFunc: Boolean, funcConstraint: Constraint)(using TypeCheckerCtx): Unit = stmts.map(check(_, isFunc = isFunc, funcConstraint))

@targetName("checkExprs")
def check(listArgs: List[Q_Expr], c: Constraint)(using TypeCheckerCtx): Option[SemType] = {
    val semTypes: List[Option[SemType]] = listArgs.map(check(_, c))

    val ty: SemType = semTypes.fold(Some(?))(_.getOrElse(?) ~ _.getOrElse(?)).getOrElse(?)

    ty.satisfies(c)
}

// This will get the most specific type out of 2 given
// This means if we know something is an Int, we can type check using that rather than ?
def mostSpecific(ty1: Option[SemType], ty2: Option[SemType]): SemType = (ty1, ty2) match {
    case (Some(?), Some(t)) => t
    case (Some(t), _)       => t
    case (None, t)          => t.getOrElse(?)
}

extension (ty: SemType) def ~(refTy: SemType): Option[SemType] = (ty, refTy) match
    case (?, refTy) => Some(refTy)
    case (ty, ?) => Some(ty)
    case (ty, refTy) if ty == refTy => Some(ty)
    case (KnownType.Array(KnownType.Char), KnownType.String) => Some(KnownType.String)
    case (KnownType.Array(ty), KnownType.Array(refTy)) =>
        ty ~ refTy
    case (KnownType.Pair(ty1, ty2), KnownType.Pair(refTy1, refTy2)) =>
        val newTy1 = ty1 ~ refTy1
        val newTy2 = ty2 ~ refTy2
        (newTy1, newTy2) match
            case (Some(t1), Some(t2)) => Some(KnownType.Pair(t1, t2))
            case _ => None
    case (X, _) => None
    case (_, X) => None
    case _ => None

extension (ty: SemType) def satisfies (c: Constraint)(using ctx: TypeCheckerCtx): Option[SemType] = (ty, c) match {
    case (ty, Constraint.Is(refTy)) => (ty ~ refTy).orElse {
        ctx.error(TypeMismatch(ty, refTy))
    }
    case (ty, Constraint.IsExactly(refTy)) => if (ty == refTy) then Some(ty) else None 
    case (?, _) => Some(?)
    case (kty@KnownType.Int, Constraint.IsNumeric) => Some(kty)
    case (kty, Constraint.IsNumeric) => ctx.error(NonNumericType(kty))
    case (kty@KnownType.Int, Constraint.IsNumericNoError) => Some(kty)
    case (kty, Constraint.IsNumericNoError) => None
    case (kty@KnownType.Char, Constraint.IsCharacter) => Some(kty)
    case (kty, Constraint.IsCharacter) => ctx.error(NonCharacterType(kty))
    case (kty@KnownType.Int, Constraint.IsNumericOrCharacter) => Some(kty)
    case (kty@KnownType.Char, Constraint.IsNumericOrCharacter) => Some(kty)
    case (kty, Constraint.IsNumericOrCharacter) => ctx.error(NonNumericType(kty))
    case (kty@KnownType.Boolean, Constraint.IsBoolean) => Some(kty)
    case (kty, Constraint.IsBoolean) => ctx.error(NonBooleanType(kty))
    case (kty@KnownType.String, Constraint.IsString) => Some(kty)
    case (kty, Constraint.IsString) => ctx.error(NonStringType(kty))
    case (kty@KnownType.Int, Constraint.IsExitable) => Some(kty)
    case (kty, Constraint.IsExitable) => ctx.error(NonExitableType(kty))
    case (kty@(KnownType.Array(_) | KnownType.Pair(_, _)), Constraint.IsFreeable) => Some(kty)
    case (kty, Constraint.IsFreeable) => ctx.error(NonFreeableType(kty))
    case (kty@(KnownType.Int | KnownType.Char), Constraint.IsReadable) => Some(kty)
    case (kty, Constraint.IsReadable) => ctx.error(NonReadableType(kty))
}


enum Constraint {
    case Is(refTy: SemType)
    case IsExactly(refTy: SemType)
    case IsNumeric
    case IsNumericNoError
    case IsCharacter
    case IsNumericOrCharacter
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
