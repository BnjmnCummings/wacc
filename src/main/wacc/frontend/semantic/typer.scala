package wacc.semantic

import wacc.*
import wacc.ast.*
import wacc.q_ast.*
import wacc.t_ast.*
import wacc.KnownType.Pair

import scala.collection.mutable
import scala.annotation.targetName

def typeCheck(prog: Q_Prog, tyInfo: TypeInfo, fname: Option[String] = None): Either[List[Err], T_Prog] = {
    // Note this List[Error] is non-empty. NonEmptyList import won't work
    // We will just return Right if there is no error hence avoids returning Left with an empty list!
    given ctx: TypeCheckerCtx = TypeCheckerCtx(tyInfo, mutable.ListBuffer(), fname)

    val progFuncs: List[Q_Func] = prog.funcs
    val progStmts: List[Q_Stmt] = prog.body
    val progScoped: Set[Name] = prog.scoped
    
    val typedFuncs = progFuncs.map(check(_, Constraint.Unconstrained)._2)
    val typedStmts = progStmts.map(check(_, isFunc = false, Constraint.Unconstrained))
    val typedScoped = progScoped.map(q_name => Name(q_name.value, q_name.num))

    ctx.errors.match {
        case err :: errs => Left(err :: errs)
        case Nil         => Right(T_Prog(typedFuncs, typedStmts, typedScoped))
    }
}

def checkDeclTypes(l: SemType, r: Q_RValue)(using ctx: TypeCheckerCtx): (Option[SemType], T_RValue) = {
    (l, r) match {
        // this breaks into inner of array then maps
        case (KnownType.Array(_), Q_ArrayLiteral(_, _)) => checkArrayDeclType(l, r.asInstanceOf[Q_ArrayLiteral])
        
        case (_, Q_Ident(n, pos)) => 
            ctx.setPos(pos)
            val t = ctx.typeOf(n)
            (l, t) match
                case (KnownType.String, KnownType.Array(KnownType.Char)) =>
                    (Some(l), T_Ident(Name(n.value, n.num)))
                case _ =>
                    if l == t then
                        (Some(l), T_Ident(Name(n.value, n.num)))
                    else
                        (ctx.error(TypeMismatch(t, l)), T_Ident(Name(n.value, n.num)))
                    
            // check(r, Constraint.IsExactly(l))
        // catches the base value case and the case where r is an ident
        case (_, _) => 
            check(r, Constraint.Is(l))
    }
}

// in the case where l = Array, r = ident => get r from typeInfo, check l IsExactly r (==)

// l != array, check l against r => l == r

// break into inner of array, map function on each inner member of literal on right

def checkArrayDeclType(l: SemType, r: Q_ArrayLiteral)(using ctx: TypeCheckerCtx): (Option[SemType], T_ArrayLiteral) = {
    ((l, r): @unchecked) match {
        case (KnownType.Array(t@KnownType.Array(_)), Q_ArrayLiteral(xs: List[Q_Expr], pos)) =>
            // l = array, r = array literal
            // so check each member of r is arrT
            // we can do this by calling checkArrayTypes with arrT and each member type of r
            ctx.setPos(pos)
            val (xsTys, xsTyped) = xs.map(check(_, Constraint.Is(t))).unzip

            val semOpt: Option[SemType] = xsTys.fold(Some(?))(_.getOrElse(?) ~ _.getOrElse(?)) // ? could get changed for arrT potentially!

            (semOpt: @unchecked) match {
                case Some(?) => (None, T_ArrayLiteral(xsTyped, ?, xs.size)) // most generic type is ? hence we don't have any common type within the right array?
                case Some(t) => (semOpt, T_ArrayLiteral(xsTyped, t, xs.size))
            }
        case (t@KnownType.Array(arrT), Q_ArrayLiteral(xs, pos)) =>
            ctx.setPos(pos)
            val (xsTys, xsTyped) = xs.map(check(_, Constraint.Unconstrained)).unzip

            val semOpt: SemType = 
                if xs.isEmpty then
                    arrT
                else
                    xsTys
                        .fold(Some(?))((t1, t2) => t1.getOrElse(?).satisfies(Constraint.Is(t2.getOrElse(?)))).getOrElse(?)
            
            arrT match
                case KnownType.String =>
                    if semOpt == KnownType.String | semOpt == KnownType.Array(KnownType.Char) then
                        (Some(t), T_ArrayLiteral(xsTyped, arrT, xs.size))
                    else
                        (ctx.error(TypeMismatch(KnownType.Array(semOpt), t)), T_ArrayLiteral(xsTyped, arrT, xs.size))
                case _ => (semOpt.satisfies(Constraint.Is(arrT)), T_ArrayLiteral(xsTyped, arrT, xs.size))
    }
}

def check(stmt: Q_Stmt, isFunc: Boolean, funcConstraint: Constraint)(using ctx: TypeCheckerCtx): T_Stmt =
    stmt match {
    case Q_Decl(id: Name, r: Q_RValue, pos) =>
        ctx.setPos(pos)

        val (declTy, declTyped) = checkDeclTypes(ctx.typeOf(id), r)

        

        T_Decl(Name(id.value, id.num), declTyped, declTy.getOrElse(?))
    // Check the type of the LValue matches that of the RValue
    case Q_Asgn(l: Q_LValue, r: Q_RValue, pos) => 
        ctx.setPos(pos)

        val (lTy, lTyped): (Option[SemType], T_LValue) = check(l, Constraint.Unconstrained)
        val _lTy = lTy.getOrElse(?)
        val (rTy, rTyped): (Option[SemType], T_RValue) = check(r, Constraint.Unconstrained)

        val ty = mostSpecific(lTy, rTy)

        (_lTy, rTy.getOrElse(?)) match {
            case (?, ?) => ctx.error(NonNumericType(?))
            case (_, _) => check(r, Constraint.Is(_lTy))
        }

        T_Asgn(lTyped, rTyped, ty)
    // Only need to  verify the LValue is actually LValue - no constraint needed? Or create constraint for IsLValue?
    case Q_Read(l: Q_LValue, pos) => 
        ctx.setPos(pos)

        val (lTy, lTyped) = check(l, Constraint.IsReadable)

        T_Read(lTyped, lTy.getOrElse(?))
    case Q_Free(x: Q_Expr, pos) => 
        ctx.setPos(pos)

        val (lTy, lTyped) = check(x, Constraint.IsFreeable)

        T_Free(lTyped, lTy.getOrElse(?))
    case Q_Return(x: Q_Expr, pos) => 
        ctx.setPos(pos)
        if isFunc then 
            val (xTy, xTyped) = check(x, funcConstraint)

            T_Return(xTyped, xTy.getOrElse(?))
        else
            ctx.error(InvalidReturn())

            val (xTy, xTyped) = check(x, Constraint.Unconstrained)

            T_Return(xTyped, xTy.getOrElse(?))
    case Q_Exit(x: Q_Expr, pos) => 
        ctx.setPos(pos)
        T_Exit(check(x, Constraint.IsExitable)._2)
    case Q_Print(x: Q_Expr, pos) => 
        ctx.setPos(pos)

        val (xTy, xTyped) = check(x, Constraint.Unconstrained)

        T_Print(xTyped, xTy.getOrElse(?))
    case Q_Println(x: Q_Expr, pos) => 
        ctx.setPos(pos)

        val (xTy, xTyped) = check(x, Constraint.Unconstrained)

        T_Println(xTyped, xTy.getOrElse(?))
    case Q_If(cond: Q_Expr, body: List[Q_Stmt], scopedBody, el: List[Q_Stmt], scopedEl, pos) =>
        ctx.setPos(pos)
        val condTyped = check(cond, Constraint.IsBoolean)._2
        val bodyTyped = check(body, isFunc, funcConstraint)
        val elTyped = check(el, isFunc, funcConstraint)
        T_If(condTyped, bodyTyped, scopedBody.map(n => Name(n.value, n.num)), elTyped, scopedEl.map(n => Name(n.value, n.num)))
    case Q_While(cond: Q_Expr, body: List[Q_Stmt], scopedBody: Set[Name], pos) =>
        ctx.setPos(pos)
        check(cond, Constraint.IsBoolean)
        T_While(check(cond, Constraint.IsBoolean)._2, check(body, isFunc, funcConstraint), scopedBody.map(n => Name(n.value, n.num)))
    case Q_CodeBlock(body: List[Q_Stmt], scopedBody: Set[Name], pos) =>
        ctx.setPos(pos)
        T_CodeBlock(check(body, isFunc, funcConstraint), scopedBody.map(n => Name(n.value, n.num)))
    case Q_Skip(_) => T_Skip()
}

def check(expr: Q_Expr, c: Constraint)(using ctx: TypeCheckerCtx): (Option[SemType], T_Expr) = expr match {
    // The below only works on two ints
    case Q_Mul(x: Q_Expr, y: Q_Expr, pos) => 
        ctx.setPos(pos)
        checkArithmeticExpr(x, y, c, T_Mul.apply)
    case Q_Div(x: Q_Expr, y: Q_Expr, pos) => 
        ctx.setPos(pos)
        checkArithmeticExpr(x, y, c, T_Div.apply)
    case Q_Mod(x: Q_Expr, y: Q_Expr, pos) => 
        ctx.setPos(pos)
        checkArithmeticExpr(x, y, c, T_Mod.apply)
    case Q_Add(x: Q_Expr, y: Q_Expr, pos) => 
        ctx.setPos(pos)
        checkArithmeticExpr(x, y, c, T_Add.apply)
    case Q_Sub(x: Q_Expr, y: Q_Expr, pos) => 
        ctx.setPos(pos)
        checkArithmeticExpr(x, y, c, T_Sub.apply)

    // The below work on two ints or two chars
    case Q_GreaterThan(x: Q_Expr, y: Q_Expr, pos) => 
        ctx.setPos(pos)
        checkComparisonExpr(x, y, c, T_GreaterThan.apply)
    case Q_GreaterThanEq(x: Q_Expr, y: Q_Expr, pos) => 
        ctx.setPos(pos)
        checkComparisonExpr(x, y, c, T_GreaterThanEq.apply)
    case Q_LessThan(x: Q_Expr, y: Q_Expr, pos) => 
        ctx.setPos(pos)
        checkComparisonExpr(x, y, c, T_LessThan.apply)
    case Q_LessThanEq(x: Q_Expr, y: Q_Expr, pos) => 
        ctx.setPos(pos)
        checkComparisonExpr(x, y, c, T_LessThanEq.apply)

    // The below work on two of the same type
    case Q_Eq(x: Q_Expr, y: Q_Expr, pos) => 
        ctx.setPos(pos)
        val (xTy, xTyped) = check(x, Constraint.Unconstrained)
        val (yTy, yTyped) = check(y, Constraint.Is(xTy.getOrElse(?)))

        val ty = mostSpecific(xTy, yTy)
        
        (Some(KnownType.Boolean), T_Eq(xTyped, yTyped, ty))
    case Q_NotEq(x: Q_Expr, y: Q_Expr, pos) => 
        ctx.setPos(pos)
        val (xTy, xTyped) = check(x, Constraint.Unconstrained)
        val (yTy, yTyped) = check(y, Constraint.Is(xTy.getOrElse(?)))

        val ty = mostSpecific(xTy, yTy)

        (Some(KnownType.Boolean), T_NotEq(xTyped, yTyped, ty))
    // The below only work on two bools
    case Q_And(x: Q_Expr, y: Q_Expr, pos) => 
        ctx.setPos(pos)
        checkBooleanExpr(x, y, c, T_And.apply)
    case Q_Or(x: Q_Expr, y: Q_Expr, pos) => 
        ctx.setPos(pos)
        checkBooleanExpr(x, y, c, T_Or.apply)

    // The below are unary
    // Int:
    case Q_Neg(x: Q_Expr, pos) =>
        ctx.setPos(pos)
        val (xTy, xTyped) = check(x, Constraint.IsNumeric)
        (xTy.getOrElse(?).satisfies(c), T_Neg(xTyped))
    case Q_Chr(x: Q_Expr, pos) =>
        ctx.setPos(pos)
        val (xTy, xTyped) = check(x, Constraint.IsNumeric)
        (KnownType.Char.satisfies(c), T_Chr(xTyped))
    // Bool:
    case Q_Not(x: Q_Expr, pos) =>
        ctx.setPos(pos)
        val (xTy, xTyped) = check(x, Constraint.IsBoolean)
        (xTy.getOrElse(?).satisfies(c), T_Not(xTyped))
    // Array of generic type:
    case Q_Len(x: Q_Expr, pos) =>
        ctx.setPos(pos)
        val (xTy, xTyped) = check(x, Constraint.IsArray)
        (KnownType.Int.satisfies(c), T_Len(xTyped))
    // Char:
    case Q_Ord(x: Q_Expr, pos) =>
        ctx.setPos(pos)
        val (xTy, xTyped) = check(x, Constraint.IsCharacter)
        (KnownType.Int.satisfies(c), T_Ord(xTyped))

    case Q_IntLiteral(v: BigInt, pos) => 
        ctx.setPos(pos)
        (KnownType.Int.satisfies(c), T_IntLiteral(v))
    case Q_BoolLiteral(v: Boolean, pos) => 
        ctx.setPos(pos)
        (KnownType.Boolean.satisfies(c), T_BoolLiteral(v))
    case Q_CharLiteral(v: Char, pos) => 
        ctx.setPos(pos)
        (KnownType.Char.satisfies(c), T_CharLiteral(v))
    case Q_StringLiteral(v: String, pos) => 
        ctx.setPos(pos)
        (KnownType.String.satisfies(c), T_StringLiteral(v))
    case Q_Ident(v: Name, pos) => 
        ctx.setPos(pos)
        (ctx.typeOf(v).satisfies(c), T_Ident(Name(v.value, v.num)))
    case Q_ArrayElem(v: Name, indices: List[Q_Expr], pos) => 
        ctx.setPos(pos)
        checkArrayElem(indices, v, c)
    case Q_PairNullLiteral => (KnownType.Pair(?, ?).satisfies(c), T_PairNullLiteral)
    case Q_PairElem(index: PairIndex, v: Q_LValue, pos) => 
        ctx.setPos(pos)
        checkPairElem(v, c)
}

def getBaseType(ty: SemType, idxRem: Integer)(using ctx: TypeCheckerCtx): Option[SemType] = (idxRem, ty) match {
    // Hit the base type
    case (0, t) => Some(t)
    // We have an array so get its type & check that
    case (n, KnownType.Array(_t)) => getBaseType(_t, n - 1)
    // We have another index but not for an array e.g. int i = 2 ; i[3]
    case (_, t) => ctx.error(InvalidIndexing())
}
 
def checkArrayElem(indices: List[Q_Expr], v: Name, c: Constraint)(using ctx: TypeCheckerCtx): (Option[SemType], T_ArrayElem) =
    // Indices should evaluate to a numberic index
    val indicesTyped = indices.map(expr => check(expr, Constraint.IsNumeric)._2)

    val t: SemType = ctx.typeOf(v) 

    // Get the base type of the array e.g. Array[Array[Bool]] -> Bool
    val checkedT = getBaseType(t, indices.length).getOrElse(?)
    
    (checkedT.satisfies(c), T_ArrayElem(Name(v.value, v.num), indicesTyped))

def checkArithmeticExpr(x: Q_Expr, y: Q_Expr, c: Constraint, result_expr: (T_Expr, T_Expr) => T_Expr)
                       (using TypeCheckerCtx): (Option[SemType], T_Expr) =
    val (xTy, xTyped) = check(x, Constraint.IsNumeric)
    val (yTy, yTyped) = check(y, Constraint.Is(xTy.getOrElse(?)))
    (mostSpecific(xTy, yTy).satisfies(c), result_expr(xTyped, yTyped))

def checkComparisonExpr(x: Q_Expr, y: Q_Expr, c: Constraint, result_expr: (T_Expr, T_Expr, SemType) => T_Expr)
                       (using TypeCheckerCtx): (Option[SemType], T_Expr) =
    val (xTy, xTyped) = check(x, Constraint.IsNumericOrCharacter)
    
    val (yTy, yTyped) = (xTy: @unchecked) match {
        case Some(KnownType.Int) => check(y, Constraint.Is(KnownType.Int))
        case Some(KnownType.Char) => check(y, Constraint.Is(KnownType.Char))
        case None => (None, result_expr(xTyped, xTyped, ?)) 
        // Note: The above result_expr can be any T_Expr - it will be disregarded as we throw an error checking xTy
    }

    val ty: SemType = mostSpecific(xTy, yTy)

    (KnownType.Boolean.satisfies(c), result_expr(xTyped, yTyped, ty))

def checkBooleanExpr(x: Q_Expr, y: Q_Expr, c: Constraint, result_expr: (T_Expr, T_Expr) => T_Expr)
                    (using TypeCheckerCtx): (Option[SemType], T_Expr) =
    val (xTy, xTyped) = check(x, Constraint.IsBoolean)
    val (yTy, yTyped) = check(y, Constraint.Is(xTy.getOrElse(?)))
    (mostSpecific(xTy, yTy).satisfies(c), result_expr(xTyped, yTyped))

def check(l: Q_LValue, c: Constraint)(using ctx: TypeCheckerCtx): (Option[SemType], T_LValue) = l match {
    case Q_Ident(v: Name, pos) => 
        ctx.setPos(pos)
        (ctx.typeOf(v).satisfies(c), T_Ident(Name(v.value, v.num)))
    case Q_PairElem(index: PairIndex, v: Q_LValue, pos) => 
        ctx.setPos(pos)
        checkPairElem(l, c)
    case Q_ArrayElem(v: Name, indices: List[Q_Expr], pos) => 
        ctx.setPos(pos)
        checkArrayElem(indices, v, c)
}

def checkPairElem(l: Q_LValue, c: Constraint)(using ctx: TypeCheckerCtx): (Option[SemType], T_PairElem) = l match {
    case Q_PairElem(index: PairIndex, v: Q_LValue, _) => 
        val (pairTy, pairTyped): (Option[SemType], T_LValue) = check(v, Constraint.Is(KnownType.Pair(?, ?)))
        
        val kt: KnownType.Pair = pairTy.getOrElse(?).asInstanceOf[KnownType.Pair]

        index match {
            case PairIndex.First  => 
                (kt.ty1.satisfies(c), T_PairElem(index, pairTyped))
            case PairIndex.Second => 
                (kt.ty2.satisfies(c), T_PairElem(index, pairTyped))
        }
    // This case should never be reached as we call checkPairElem on pair elems only
    case _ => throw Exception("tried to use checkPairElem on something other than a Q_PairElem")
}

def check(r: Q_RValue, c: Constraint)(using ctx: TypeCheckerCtx): (Option[SemType], T_RValue) =
    r match {
    case Q_FuncCall(v: Name, args: List[Q_Expr], pos) =>
        ctx.setPos(pos)
        val returnType: KnownType = ctx.typeOfFunc(v)._1
        val argNames: List[Name] = ctx.typeOfFunc(v)._2

        var argsTyped: List[T_Expr] = List()

        if (args.length != argNames.length) then
            ctx.error(WrongNumberOfArgs(args.length, argNames.length))
        else
            // zip with list of numbers [0,1..]
            argsTyped = args
                .zip(argNames)
                .map((arg, name) => {
                    val expectedType: KnownType = ctx.typeOf(name)
                    check(arg, Constraint.Is(expectedType))._2
                })

        (returnType.satisfies(c), T_FuncCall(Name(v.value, v.num), argsTyped))
    case Q_ArrayLiteral(xs: List[Q_Expr], _) =>
        val xs_processed = xs.map(check(_, Constraint.Unconstrained))
        val xs_typed = xs_processed.map(_._2)
        val ty = xs_processed.map(_._1)
            .fold(Some(?))((t1, t2) => t1.getOrElse(?).satisfies(Constraint.Is(t2.getOrElse(?)))).getOrElse(X)

        (KnownType.Array(ty).satisfies(c), T_ArrayLiteral(xs_typed, ty, xs.size))
    case Q_PairElem(index: PairIndex, v: Q_LValue, pos) =>
        ctx.setPos(pos)  
        val (pairTy, pair_typed): (Option[SemType], T_LValue) = check(v, Constraint.Is(KnownType.Pair(?, ?)))
        val kt: KnownType.Pair = pairTy.getOrElse(?).asInstanceOf[KnownType.Pair]

        index match {
            case PairIndex.First  => 
                (kt.ty1.satisfies(c), T_PairElem(index, pair_typed))
            case PairIndex.Second => 
                (kt.ty2.satisfies(c), T_PairElem(index, pair_typed))
        }
    case Q_NewPair(x: Q_Expr, y: Q_Expr, pos) =>
        ctx.setPos(pos)
        val (xTy, xTyped) = check(x, Constraint.Unconstrained)
        val (yTy, yTyped) = check(y, Constraint.Unconstrained)
        (KnownType.Pair(xTy.getOrElse(?), yTy.getOrElse(?)).satisfies(c), T_NewPair(xTyped, yTyped, xTy.getOrElse(?), yTy.getOrElse(?)))
    case e: Q_Expr => check(e, c)
}

def checkReturnType(t: Type, stmt: Q_Stmt)(using ctx: TypeCheckerCtx): Option[SemType] = (stmt, t) match {
    case (Q_Return(x: Q_Expr, pos), ty) => 
        ctx.setPos(pos)
        check(x, Constraint.Is(toSemType(ty)))._1
    case (Q_Exit(x: Q_Expr, pos), _) => 
        ctx.setPos(pos)
        check(x, Constraint.Is(KnownType.Int))._1
    case (Q_If(cond: Q_Expr, body: List[Q_Stmt], _, el: List[Q_Stmt], _, pos), t) =>
        ctx.setPos(pos)
        Some(mostSpecific(checkReturnType(t, body.last), checkReturnType(t, el.last)))
    case (Q_CodeBlock(stmts: List[Q_Stmt], _, pos), t) => 
        ctx.setPos(pos)
        checkReturnType(t, stmts.last)
    case (_, _) => throw SyntaxFailureException("Last statement is not a return/if. This should be dealt with in parsing")
}

def check(func: Q_Func, c: Constraint)(using ctx: TypeCheckerCtx): (Option[SemType], T_Func) = {
    val typedArgs = func.args.map(q_param => T_Param(q_param.t, Name(q_param.v.value, q_param.v.num)))
    val typedBody = func.body.map(check(_, isFunc = true, Constraint.Is(toSemType(func.t))))

    (checkReturnType(func.t, func.body.last), T_Func(func.t, Name(func.v.value, func.v.num), typedArgs, typedBody, func.scoped.map(q_name => Name(q_name.value, q_name.num))))
}

@targetName("checkStmts")
def check(stmts: List[Q_Stmt], isFunc: Boolean, funcConstraint: Constraint)(using TypeCheckerCtx): List[T_Stmt] = stmts.map(check(_, isFunc = isFunc, funcConstraint))

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
    case (Pair(?, ?), Constraint.Is(KnownType.Pair(_, _))) => Some(Pair(?, ?))
    case (ty, Constraint.Is(refTy)) => (ty ~ refTy).orElse {
        ctx.error(TypeMismatch(ty, refTy))
    }
    case (ty, Constraint.IsExactly(refTy)) => if (ty == refTy) then Some(ty) else None 
    case (?, Constraint.IsReadable) => ctx.error(NonReadableType(?))
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
    case (kty@KnownType.Pair(_, _), Constraint.IsPairNoError) => Some(kty)
    case (kty, Constraint.IsPairNoError) => None
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
    case IsPairNoError
}

object Constraint {
    val Unconstrained = Is(?) // Always passes
    val IsArray = Is(KnownType.Array(?))
    val IsPair = Is(KnownType.Pair(?, ?))
}
