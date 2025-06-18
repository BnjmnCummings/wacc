package wacc.semantic

import wacc.ast.*
import wacc.q_ast.*
import wacc.t_ast.*
import wacc.error.*

import scala.collection.mutable
import scala.annotation.targetName

/**
  * A function to check if a program has valid typing.
  * @param prog the qualified (scope checked) AST.
  * @param tyInfo type information about the functions and variables in scope.
  * @param fname the fileName of the wacc program (used for error handling).
  * @return a typed program [[T_Prog]] if there are no errors thrown.
  */
def typeCheck(prog: Q_Prog, tyInfo: TypeInfo, fname: Option[String] = None): Either[List[Err], T_Prog] = 
    given ctx: TypeCheckerCtx = TypeCheckerCtx(tyInfo, mutable.ListBuffer(), fname)

    val progFuncs: List[Q_Func] = prog.funcs
    val progStmts: List[Q_Stmt] = prog.body
    val progScoped: Set[Name] = prog.scoped
    
    val typedFuncs = progFuncs.map(check(_, Constraint.Unconstrained)._2)
    val typedStmts = progStmts.map(check(_, isFuncBody = false, Constraint.Unconstrained))
    val typedScoped = progScoped.map(q_name => Name(q_name.value, q_name.num))

    ctx.getErrors.match
        case err :: errs => Left(err :: errs)
        case Nil         => Right(T_Prog(typedFuncs, typedStmts, typedScoped))
    
/**
  * Checks a function against a given constraint.
  * @param func the qualified function (scope checked).
  * @param c the function constraint.
  * @return (the type of the function, the typed function [[T_Func]])
  */
def check(func: Q_Func, c: Constraint)(using ctx: TypeCheckerCtx): (Option[SemType], T_Func) = 
    val typedArgs = func.args.map { q_param => 
        T_Param(q_param.t, Name(q_param.name.value, q_param.name.num))
    }
    val typedBody = func.body.map { 
        check(_, isFuncBody = true, Constraint.Is(toSemType(func.t))) 
    } 
    val typedFunc = T_Func(
        func.t, 
        Name(func.name.value, func.name.num), 
        typedArgs, 
        typedBody, 
        func.scoped.map { q_name => Name(q_name.value, q_name.num)} 
    )

    (checkReturnType(func.t, func.body.last), typedFunc)

/**
  * Checks a list of statements against a given constraint.
  * @param stmts the list of qualified statements (scope checked).
  * @param isFuncBody true if the statements are inside a function body.
  * @param funcConstraint the function constraint.
  * @return a list of typed statements [[List[T_Stmt]]]
  */
@targetName("checkStmts")
def check(stmts: List[Q_Stmt], isFuncBody: Boolean, funcConstraint: Constraint)(using TypeCheckerCtx): List[T_Stmt] = 
    stmts.map(check(_, isFuncBody = isFuncBody, funcConstraint))

/**
  * Checks a statement against a given constraint.
  * @param stmt the qualified statement (scope checked).
  * @param isFuncBody true if the statement is inside a function body.
  * @param funcConstraint the function constraint.
  * @return a typed statement [[T_Stmt]]
  */
def check(stmt: Q_Stmt, isFuncBody: Boolean, funcConstraint: Constraint)(using ctx: TypeCheckerCtx): T_Stmt = stmt match 
    case Q_Decl(id: Name, r: Q_RValue, pos) =>
        ctx.setPos(pos)

        val (declTy, declTyped) = checkDeclTypes(ctx.typeOf(id), r)

        T_Decl(Name(id.value, id.num), declTyped, declTy.getOrElse(?))

    /* Check the type of the LValue matches that of the RValue */
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

        if isFuncBody then 
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
        val bodyTyped = check(body, isFuncBody, funcConstraint)
        val elTyped = check(el, isFuncBody, funcConstraint)

        T_If(condTyped, bodyTyped, scopedBody.map(n => Name(n.value, n.num)), elTyped, scopedEl.map(n => Name(n.value, n.num)))

    case Q_While(cond: Q_Expr, body: List[Q_Stmt], scopedBody: Set[Name], pos) =>
        ctx.setPos(pos)

        check(cond, Constraint.IsBoolean)

        T_While(check(cond, Constraint.IsBoolean)._2, check(body, isFuncBody, funcConstraint), scopedBody.map(n => Name(n.value, n.num)))

    case Q_CodeBlock(body: List[Q_Stmt], scopedBody: Set[Name], pos) =>
        ctx.setPos(pos)
        T_CodeBlock(check(body, isFuncBody, funcConstraint), scopedBody.map(n => Name(n.value, n.num)))

    case Q_Skip(_) => T_Skip()

/**
  * Checks an l-value  against a given constraint.
  * @param l the qualified l-value (scope checked).
  * @param c the constraint.
  * @return (the type of the l-value, the typed lvalue [[T_LValue]])
  */
def check(l: Q_LValue, c: Constraint)(using ctx: TypeCheckerCtx): (Option[SemType], T_LValue) = l match 
    case Q_Ident(v: Name, pos) => 
        ctx.setPos(pos)
        (ctx.typeOf(v).satisfies(c), T_Ident(Name(v.value, v.num)))

    case Q_PairElem(index: PairIndex, v: Q_LValue, pos) => 
        ctx.setPos(pos)
        checkPairElem(index, v, c)

    case Q_ArrayElem(v: Name, indices: List[Q_Expr], pos) => 
        ctx.setPos(pos)
        checkArrayElem(indices, v, c)

/**
  * Checks an r-value  against a given constraint.
  * @param r the qualified r-value (scope checked).
  * @param c the constraint.
  * @return (the type of the r-value, the typed r-value [[T_RValue]])
  */
def check(r: Q_RValue, c: Constraint)(using ctx: TypeCheckerCtx): (Option[SemType], T_RValue) = r match 
    case Q_FuncCall(v: Name, args: List[Q_Expr], pos) =>
        ctx.setPos(pos)

        val returnType: KnownType = ctx.typeOfFunc(v)._1
        val argNames: List[Name] = ctx.typeOfFunc(v)._2
        var typedArgs: List[T_Expr] = List()

        if (args.length != argNames.length) then
            ctx.error(WrongNumberOfArgs(args.length, argNames.length))
        else 
            typedArgs = args.zip(argNames).map { (arg, name) => 
                val expectedType: KnownType = ctx.typeOf(name)
                check(arg, Constraint.Is(expectedType))._2
            }

        (returnType.satisfies(c), T_FuncCall(Name(v.value, v.num), typedArgs))

    case Q_ArrayLiteral(xs: List[Q_Expr], pos) =>
        val xs_processed = xs.map(check(_, Constraint.Unconstrained))
        val xs_typed = xs_processed.map(_._2)
        val ty = xs_processed.map(_._1)
            .fold(Some(?))((t1, t2) => t1.getOrElse(?).satisfies(Constraint.Is(t2.getOrElse(?)))).getOrElse(X)

        ctx.setPos(pos) 

        (KnownType.Array(ty).satisfies(c), T_ArrayLiteral(xs_typed, ty, xs.size))

    case Q_PairElem(index: PairIndex, v: Q_LValue, pos) =>
        val (pairTy, pair_typed): (Option[SemType], T_LValue) = check(v, Constraint.Is(KnownType.Pair(?, ?)))
        val kt: KnownType.Pair = pairTy.getOrElse(?).asInstanceOf[KnownType.Pair]

        ctx.setPos(pos) 

        index match 
            case PairIndex.First  => (kt.ty1.satisfies(c), T_PairElem(index, pair_typed))
            case PairIndex.Second => (kt.ty2.satisfies(c), T_PairElem(index, pair_typed))
        
    case Q_NewPair(x: Q_Expr, y: Q_Expr, pos) =>
        val (xTy, xTyped) = check(x, Constraint.Unconstrained)
        val (yTy, yTyped) = check(y, Constraint.Unconstrained)

        ctx.setPos(pos)

        (KnownType.Pair(xTy.getOrElse(?), yTy.getOrElse(?)).satisfies(c), T_NewPair(xTyped, yTyped, xTy.getOrElse(?), yTy.getOrElse(?)))


    case e: Q_Expr => check(e, c)

/**
  * Checks an expression against a given constraint.
  * @param expr the qualified expression.
  * @param c the constraint.
  * @return (the type of the expression, the typed expression [[T_Expr]])
  */
def check(expr: Q_Expr, c: Constraint)(using ctx: TypeCheckerCtx): (Option[SemType], T_Expr) = expr match
    /* literals */
    case Q_IntLiteral(v: BigInt, pos)    => {ctx.setPos(pos); (KnownType.Int.satisfies(c), T_IntLiteral(v))}
    case Q_BoolLiteral(v: Boolean, pos)  => {ctx.setPos(pos); (KnownType.Boolean.satisfies(c), T_BoolLiteral(v))}
    case Q_CharLiteral(v: Char, pos)     => {ctx.setPos(pos); (KnownType.Char.satisfies(c), T_CharLiteral(v))}
    case Q_StringLiteral(v: String, pos) => {ctx.setPos(pos); (KnownType.String.satisfies(c), T_StringLiteral(v))}
    case Q_PairNullLiteral               => (KnownType.Pair(?, ?).satisfies(c), T_PairNullLiteral)

    /* l-values */
    case Q_Ident(v: Name, pos)                            => {ctx.setPos(pos); (ctx.typeOf(v).satisfies(c), T_Ident(Name(v.value, v.num)))}
    case Q_ArrayElem(v: Name, indices: List[Q_Expr], pos) => {ctx.setPos(pos); checkArrayElem(indices, v, c)}
    case Q_PairElem(index: PairIndex, v: Q_LValue, pos)   => {ctx.setPos(pos); checkPairElem(index, v, c)}

    /* binary operations on integers */
    case Q_Mul(x: Q_Expr, y: Q_Expr, pos) => {ctx.setPos(pos); checkArithmeticExpr(x, y, c, T_Mul.apply)}
    case Q_Div(x: Q_Expr, y: Q_Expr, pos) => {ctx.setPos(pos); checkArithmeticExpr(x, y, c, T_Div.apply)}
    case Q_Mod(x: Q_Expr, y: Q_Expr, pos) => {ctx.setPos(pos); checkArithmeticExpr(x, y, c, T_Mod.apply)}
    case Q_Add(x: Q_Expr, y: Q_Expr, pos) => {ctx.setPos(pos); checkArithmeticExpr(x, y, c, T_Add.apply)}
    case Q_Sub(x: Q_Expr, y: Q_Expr, pos) => {ctx.setPos(pos); checkArithmeticExpr(x, y, c, T_Sub.apply)}

    /* binary operations on integers or chars (both must be the same type) */
    case Q_GreaterThan(x: Q_Expr, y: Q_Expr, pos)   => {ctx.setPos(pos); checkComparisonExpr(x, y, c, T_GreaterThan.apply)}
    case Q_GreaterThanEq(x: Q_Expr, y: Q_Expr, pos) => {ctx.setPos(pos); checkComparisonExpr(x, y, c, T_GreaterThanEq.apply)}
    case Q_LessThan(x: Q_Expr, y: Q_Expr, pos)      => {ctx.setPos(pos); checkComparisonExpr(x, y, c, T_LessThan.apply)}
    case Q_LessThanEq(x: Q_Expr, y: Q_Expr, pos)    => {ctx.setPos(pos); checkComparisonExpr(x, y, c, T_LessThanEq.apply)}

    /* binary operations on bools */
    case Q_And(x: Q_Expr, y: Q_Expr, pos) => {ctx.setPos(pos); checkBooleanExpr(x, y, c, T_And.apply)}
    case Q_Or(x: Q_Expr, y: Q_Expr, pos)  => {ctx.setPos(pos); checkBooleanExpr(x, y, c, T_Or.apply)}

    /* binary operations on two of the same type */
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

    /* unary operations on integers */
    case Q_Neg(x: Q_Expr, pos) =>
        ctx.setPos(pos)
        val (xTy, xTyped) = check(x, Constraint.IsNumeric)
        (xTy.getOrElse(?).satisfies(c), T_Neg(xTyped))

    case Q_Chr(x: Q_Expr, pos) =>
        ctx.setPos(pos)
        val (xTy, xTyped) = check(x, Constraint.IsNumeric)
        (KnownType.Char.satisfies(c), T_Chr(xTyped))


    /* unary operations on bools */
    case Q_Not(x: Q_Expr, pos) =>
        ctx.setPos(pos)
        val (xTy, xTyped) = check(x, Constraint.IsBoolean)
        (xTy.getOrElse(?).satisfies(c), T_Not(xTyped))

    /* unary operations on arrays */
    case Q_Len(x: Q_Expr, pos) =>
        ctx.setPos(pos)
        val (xTy, xTyped) = check(x, Constraint.IsArray)
        (KnownType.Int.satisfies(c), T_Len(xTyped))

    /* unary operations on chars */
    case Q_Ord(x: Q_Expr, pos) =>
        ctx.setPos(pos)
        val (xTy, xTyped) = check(x, Constraint.IsCharacter)
        (KnownType.Int.satisfies(c), T_Ord(xTyped))
    
/**
  * Checks the types for a declaration/assignment.
  * @param lType the type of the l-value
  * @param r the r-value we are trying to store.
  * @return (the type of the declaration, the rvalue as a [[T_RValue]])
  */
def checkDeclTypes(lType: SemType, r: Q_RValue)(using ctx: TypeCheckerCtx): (Option[SemType], T_RValue) = (lType, r) match
    case (KnownType.Array(_), Q_ArrayLiteral(_, _)) => checkArrayDeclType(lType, r.asInstanceOf[Q_ArrayLiteral])    
    case (_, Q_Ident(n, pos)) => 
        ctx.setPos(pos)

        val t = ctx.typeOf(n)

        (lType, t) match
            case (KnownType.String, KnownType.Array(KnownType.Char)) => (Some(lType), T_Ident(Name(n.value, n.num)))
            case _ =>
                if lType == t then
                    (Some(lType), T_Ident(Name(n.value, n.num)))
                else
                    (ctx.error(TypeMismatch(t, lType)), T_Ident(Name(n.value, n.num)))        

    /* catches the base value case and the case where r is an ident */
    case (_, _) => check(r, Constraint.Is(lType))

/**
  * Checks the type of an array literal during declaration. 
  * @param l the type of the declaration.
  * @param r the qualified array literal.
  * @return (the type of the declaration, the rvalue as a [[T_ArrayLiteral]])
  */
def checkArrayDeclType(l: SemType, r: Q_ArrayLiteral)(using ctx: TypeCheckerCtx): (Option[SemType], T_ArrayLiteral) = ((l, r): @unchecked) match
    case (KnownType.Array(t@KnownType.Array(_)), Q_ArrayLiteral(xs, pos)) =>
        ctx.setPos(pos)

        val (xsTys, xsTyped) = xs.map(check(_, Constraint.Is(t))).unzip
        val semOpt: Option[SemType] = xsTys.fold(Some(?))(_.getOrElse(?) ~ _.getOrElse(?))

        (semOpt: @unchecked) match 
            case Some(?) => (None, T_ArrayLiteral(xsTyped, ?, xs.size))
            case Some(t) => (semOpt, T_ArrayLiteral(xsTyped, t, xs.size))

    case (t@KnownType.Array(arrT), Q_ArrayLiteral(xs, pos)) =>
        ctx.setPos(pos)

        val (xsTys, xsTyped) = xs.map(check(_, Constraint.Unconstrained)).unzip
        val semOpt: SemType = 
            if xs.isEmpty then
                arrT
            else
                xsTys.fold(Some(?)) { (t1, t2) => 
                    t1.getOrElse(?).satisfies(Constraint.Is(t2.getOrElse(?)))
                }.getOrElse(?)       
                         
        arrT match
            case KnownType.String =>
                if semOpt == KnownType.String | semOpt == KnownType.Array(KnownType.Char) then
                    (Some(t), T_ArrayLiteral(xsTyped, arrT, xs.size))
                else
                    (ctx.error(TypeMismatch(KnownType.Array(semOpt), t)), T_ArrayLiteral(xsTyped, arrT, xs.size))

            case _ => (semOpt.satisfies(Constraint.Is(arrT)), T_ArrayLiteral(xsTyped, arrT, xs.size))
    
/**
  * Checks an array element call.
  * @param indices list of the nested indicies pointing to the element.
  * @param v the name of the array.
  * @param c the constraint.
  * @return (the type of the array, the rvalue as a [[T_ArrayElem]])
  */
def checkArrayElem(indices: List[Q_Expr], v: Name, c: Constraint)(using ctx: TypeCheckerCtx): (Option[SemType], T_ArrayElem) =
    val indicesTyped = indices.map { expr => check(expr, Constraint.IsNumeric)._2 } // Indices should evaluate to a numberic index
    val t: SemType = ctx.typeOf(v) 
    val checkedT = getBaseType(t, indices.length).getOrElse(?)
    
    (checkedT.satisfies(c), T_ArrayElem(Name(v.value, v.num), indicesTyped))

/**
  * Another function that unwraps an array type.
  * @param ty The outside type of the array.
  * @param idxRem indicies remaining (how many layers of nested arrays we have left).
  */
private def getBaseType(ty: SemType, idxRem: Integer)(using ctx: TypeCheckerCtx): Option[SemType] = (idxRem, ty) match
    case (0, t) => Some(t)
    case (n, KnownType.Array(_t)) => getBaseType(_t, n - 1)
    case (_, t) => ctx.error(InvalidIndexing())

/**
  * Checks that a Arithmetic binary operation is correctly typed.
  * @param x the first operand.
  * @param y the second operand.
  * @param c the constraint
  * @param result_expr the operation.
  * @return (the type of the expression, the expression as a [[T_Expr]])
  */
def checkArithmeticExpr(x: Q_Expr, y: Q_Expr, c: Constraint, result_expr: (T_Expr, T_Expr) => T_Expr)(using TypeCheckerCtx): (Option[SemType], T_Expr) =
    val (xTy, xTyped) = check(x, Constraint.IsNumeric)
    val (yTy, yTyped) = check(y, Constraint.Is(xTy.getOrElse(?)))

    (mostSpecific(xTy, yTy).satisfies(c), result_expr(xTyped, yTyped))

/**
  * Checks that a Comparison binary operation is correctly typed.
  * @param x the first operand.
  * @param y the second operand.
  * @param c the constraint
  * @param result_expr the operation.
  * @return (the type of the expression, the expression as a [[T_Expr]])
  */
def checkComparisonExpr(x: Q_Expr, y: Q_Expr, c: Constraint, result_expr: (T_Expr, T_Expr, SemType) => T_Expr)(using TypeCheckerCtx): (Option[SemType], T_Expr) =
    val (xTy, xTyped) = check(x, Constraint.IsNumericOrCharacter)
    val (yTy, yTyped) = (xTy: @unchecked) match 
        case Some(KnownType.Int) => check(y, Constraint.Is(KnownType.Int))
        case Some(KnownType.Char) => check(y, Constraint.Is(KnownType.Char))
        case None => (None, result_expr(xTyped, xTyped, ?)) // This can be any T_Expr - it will be disregarded as we throw an error checking xTy

    val ty: SemType = mostSpecific(xTy, yTy)

    (KnownType.Boolean.satisfies(c), result_expr(xTyped, yTyped, ty))

/**
  * Checks that a Boolean binary operation is correctly typed.
  * @param x the first operand.
  * @param y the second operand.
  * @param c the constraint
  * @param result_expr the operation.
  * @return (the type of the expression, the expression as a [[T_Expr]])
  */
def checkBooleanExpr(x: Q_Expr, y: Q_Expr, c: Constraint, result_expr: (T_Expr, T_Expr) => T_Expr)(using TypeCheckerCtx): (Option[SemType], T_Expr) =
    val (xTy, xTyped) = check(x, Constraint.IsBoolean)
    val (yTy, yTyped) = check(y, Constraint.Is(xTy.getOrElse(?)))

    (mostSpecific(xTy, yTy).satisfies(c), result_expr(xTyped, yTyped))

/**
  * Checks that a pair elem call is a pair type.
  * @param index the pair index.
  * @param v the l-value to be queried.
  * @param c the constraint.
  * @return (the type of the element, the pair element as a [[T_PairElem]])
  */
def checkPairElem(index: PairIndex, v: Q_LValue, c: Constraint)(using ctx: TypeCheckerCtx): (Option[SemType], T_PairElem) =
    val (pairTy, pairTyped): (Option[SemType], T_LValue) = check(v, Constraint.Is(KnownType.Pair(?, ?)))
        val kt: KnownType.Pair = pairTy.getOrElse(?).asInstanceOf[KnownType.Pair]

        index match
            case PairIndex.First  => 
                (kt.ty1.satisfies(c), T_PairElem(index, pairTyped))
            case PairIndex.Second => 
                (kt.ty2.satisfies(c), T_PairElem(index, pairTyped))

/**
  * Checks that a return type of a fucntion matches the type of the return call.
  * @param t the expected type.
  * @param stmt the 'returning' statement.
  * @return the return type.
  */
def checkReturnType(t: Type, stmt: Q_Stmt)(using ctx: TypeCheckerCtx): Option[SemType] = stmt match
    case Q_Return(x: Q_Expr, pos)                 => {ctx.setPos(pos); check(x, Constraint.Is(toSemType(t)))._1}
    case Q_Exit(x: Q_Expr, pos)                   => {ctx.setPos(pos); check(x, Constraint.Is(KnownType.Int))._1}
    case Q_CodeBlock(stmts: List[Q_Stmt], _, pos) => {ctx.setPos(pos); checkReturnType(t, stmts.last)}

    case Q_If(cond: Q_Expr, body: List[Q_Stmt], _, el: List[Q_Stmt], _, pos) =>
        ctx.setPos(pos)
        Some(mostSpecific(checkReturnType(t, body.last), checkReturnType(t, el.last)))
    
    case _ => throw SyntaxFailureException("Last statement is not a return/if. This should be dealt with in parsing")

/**
  * This will get the most specific type out of 2 given.
  * This means if we know something is an Int, we can type check using that rather than ?.
  * @param ty1 the first type.
  * @param ty2 the second type.
  */
def mostSpecific(ty1: Option[SemType], ty2: Option[SemType]): SemType = (ty1, ty2) match
    case (Some(t), _)       => t
    case (_, Some(t))       => t
    case _                  => ?

/**
  * An extension method for refining types. 
  * Allows for char arrays to be turned into strings etc.
  * @param refTy the refined type.
  */
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
