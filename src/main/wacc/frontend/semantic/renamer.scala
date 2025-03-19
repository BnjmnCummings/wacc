package wacc

import wacc.ast.*
import wacc.q_ast.*

import collection.immutable.Set as Set
import collection.immutable.Map as Map
import collection.mutable.Set as MutableSet
import collection.mutable.Map as MutableMap
import collection.mutable.ListBuffer

object renamer {
    private var funcNames = MutableSet[Name]()
    private var nameGenTable = MutableMap[String, Int]()
    private var varTypes = MutableMap[Name, SemType]()
    private var funcTypes = MutableMap[Name, (SemType, List[Name])]()

    /**
      * A function to 'scope-check' the identities in our program by renaming.
      * @param prog the complete AST of the parsed program.
      * @param filename the filename of the .wacc program.
      * @return (a qualified program, type information about vars and funcs).
      */
    def rename(prog: Prog, filename: Option[String] = None): (Q_Prog, TypeInfo) =
        given ctx: RenamerContext = RenamerContext(fnameIn = filename)
        
        funcNames = MutableSet()
        nameGenTable = MutableMap()
        varTypes = MutableMap()
        funcTypes = MutableMap()

        /* it is important this occurs first as it adds functions to funcNames */
        val _funcs = rename(prog.funcs)
        val (_body, scoped) = rename(prog.body, Set(), Set())
        val (_varTypes, _funcTypes) = verifyTyped(varTypes, funcTypes)

        (Q_Prog(_funcs, _body, scoped), TypeInfo(_varTypes, _funcTypes))

    /**
      * Renames a list of functions.
      * @param funcs the list of functions.
      * @return a list of qualified functions.
      */
    private def rename(funcs: List[Func])(using ctx: RenamerContext): List[Q_Func] = 
        val data: List[(Name, List[Q_Param])] = funcs.map(initialiseFunc)
        val _funcs: ListBuffer[Q_Func] = ListBuffer()

        funcs.zip(data).foreach {
            case (func, (v, args)) => 
                val _func = rename(func, v, args)
                
                funcNames += _func.name
                _funcs += _func
        }

        _funcs.toList

    /**
      * A helper function for generating the function names and scope checking the arguments.
      * @note This needs to be done before renaming occurs.
      * @param func the function.
      * @return (renamed function, list of qualified parameters).
      */
    private def initialiseFunc(func: Func)(using ctx: RenamerContext): (Name, List[Q_Param]) =
        if funcNames.exists(_.value == func.name) then
            ctx.setPos(func.pos)
            ctx.errors += ScopeError(s"function ${func.name} already declared in scope")
            throw ScopeException(ctx.errors.toList)

        val v: Name = genName(func.name)
        val args: ListBuffer[Q_Param] = ListBuffer()

        for (arg <- func.args) 
            val _arg = rename(arg, args.map(_.name).toSet)
            args += _arg
        
        newFunc(v, func.t, args.toList.map(_.name))

        (v, args.toList)

    /**
      * Renames a single function.
      * @param func the function.
      * @param v the new name of the function.
      * @param args the arguments.
      * @return a qualified function.
      */
    private def rename(func: Func, v: Name, args: List[Q_Param])(using ctx: RenamerContext): Q_Func =
        val (body, scoped) = rename(func.body, args.map(_.name).toSet, Set())
        Q_Func(func.t, v, args, body, scoped, func.pos)
    
    /**
      * Renames a function parameter.
      * @param param the parameter.
      * @param lScope a set of the 'other parameters' for this function.
      * @return a qualified parameter.
      */
    private def rename(param: Param, lScope: Set[Name])(using ctx: RenamerContext): Q_Param = 
        if lScope.exists(_.value == param.name) then
            ctx.setPos(param.pos)
            ctx.errors += ScopeError(s"variable ${param.name} already declared in scope")
            throw ScopeException(ctx.errors.toList)

        Q_Param(param.t, newVar(param.name, Some(param.t)), param.pos)
    
    /**
      * Renames a list of statements.
      * @param stmts the list of statements.
      * @param pScope the 'parent scope', all the variables we have access to. 
      * @param lScope the scope of the statements.
      * @return (list of qualified statements, the new scope) 
      */
    private def rename(stmts: List[Stmt], pScope: Set[Name], lScope: Set[Name])(using ctx: RenamerContext): (List[Q_Stmt], Set[Name]) = 
        val _lScope: MutableSet[Name] = MutableSet()
        _lScope ++= lScope

        val _stmts: ListBuffer[Q_Stmt] = ListBuffer()
        for (stmt <- stmts) 
            val _stmt = rename(stmt, pScope, _lScope.toSet)
            _stmts += _stmt
            _stmt match 
                case Q_Decl(v, _, _) => _lScope += v
                case _               => ()
            
        (_stmts.toList, _lScope.toSet)
    
    /**
      * Renames a single statement.
      * @param stmt the statement.
      * @param pScope the 'parent scope'
      * @param lScope the scope of the statements.
      * @return a qualified statement.
      */
    private def rename(stmt: Stmt, pScope: Set[Name], lScope: Set[Name])(using ctx: RenamerContext): Q_Stmt = stmt match
        case Skip(pos)       => Q_Skip(pos)
        case asgn@Asgn(l, r) => Q_Asgn(rename(l, merge(lScope, pScope)), rename(r, merge(lScope, pScope)), asgn.pos)
        case read@Read(l)    => Q_Read(rename(l, merge(lScope, pScope)), read.pos)
        case free@Free(x)    => Q_Free(rename(x, merge(lScope, pScope)), free.pos)
        case ret@Return(x)   => Q_Return(rename(x, merge(lScope, pScope)), ret.pos)
        case ex@Exit(x)      => Q_Exit(rename(x, merge(lScope, pScope)), ex.pos)
        case pr@Print(x)     => Q_Print(rename(x, merge(lScope, pScope)), pr.pos)
        case prl@Println(x)  => Q_Println(rename(x, merge(lScope, pScope)), prl.pos)

        case ifEl@If(cond, body, el) => 
            val (_body, scopedBody) = rename(body, merge(lScope, pScope), Set())
            val (_el, scopedEl) = rename(el, merge(lScope, pScope), Set())
            Q_If(rename(cond, merge(lScope, pScope)), _body, scopedBody, _el, scopedEl, ifEl.pos)

        case whl@While(cond, body) => 
            val (_body, scoped) = rename(body, lScope ++ pScope, Set())
            Q_While(rename(cond, merge(lScope, pScope)), _body, scoped, whl.pos)

        case cBlock@CodeBlock(body) =>
            val (_body, scoped) = rename(body, merge(lScope, pScope), Set())
            Q_CodeBlock(_body, scoped, cBlock.pos)
        
        case decl@Decl(t, Ident(v), r) => 
            val rvalue = rename(r, merge(lScope, pScope)) //need to evaluate r-value first so that we can't declare an ident as itself
            
            if lScope.exists(_.value == v) then
                val var_name = lScope.find(_.value == v).get

                if varTypes(var_name) == ? then
                    updateType(var_name, toSemType(t))
                else
                    ctx.setPos(decl.pos)
                    ctx.errors += ScopeError(s"variable $v already declared in scope")
                    throw ScopeException(ctx.errors.toList)

            Q_Decl(newVar(v, Some(t)), rvalue, decl.pos)

    /**
      * Renames an l-vlaue.
      * @param lvalue the l-value.
      * @param scope the scope we are currently in.
      * @return a qualified l-value
      */
    private def rename(lvalue: LValue, scope: Set[Name])(using ctx: RenamerContext): Q_LValue = lvalue match
        case id@Ident(v) => rename(v, scope, id.pos) // if the identity for an l-value doesn't yet exist, complain.
        case arr@ArrayElem(v, indicies) => 
            if (!scope.exists(_.value == v)) then
                ctx.setPos(arr.pos)
                ctx.errors += ScopeError(s"variable $v not declared in scope")
                throw ScopeException(ctx.errors.toList)
            
            Q_ArrayElem(getName(v, scope), indicies.map(rename(_, scope)), arr.pos)
        
        case pElem@PairElem(index, v) => Q_PairElem(index, rename(v, scope), pElem.pos)

    /**
      * Renames an r-vlaue.
      * @param rvalue the r-value.
      * @param scope the scope we are currently in.
      * @return a qualified r-value
      */
    private def rename(rvalue: RValue, scope: Set[Name])(using ctx: RenamerContext): Q_RValue = rvalue match
        case expr: Expr              => rename(expr, scope)
        case fCall@FuncCall(v, args) => Q_FuncCall(getFuncName(v), args.map(rename(_, scope)), fCall.pos)
        case aLit@ArrayLiteral(xs)   => Q_ArrayLiteral(xs.map(rename(_, scope)), aLit.pos)
        case nPair@NewPair(x1, x2)   => Q_NewPair(rename(x1, scope), rename(x2, scope), nPair.pos)

    /**
      * Renames an expression.
      * @param expr the expression.
      * @param scope the scope we are in.
      * @return a qualified expression.
      */
    private def rename(expr: Expr, scope: Set[Name])(using ctx: RenamerContext): Q_Expr = expr match
        case mul@Mul(x, y)              => Q_Mul(rename(x, scope), rename(y, scope), mul.pos)
        case mod@Mod(x, y)              => Q_Mod(rename(x, scope), rename(y, scope), mod.pos)
        case add@Add(x, y)              => Q_Add(rename(x, scope), rename(y, scope), add.pos)
        case div@Div(x, y)              => Q_Div(rename(x, scope), rename(y, scope), div.pos)
        case sub@Sub(x, y)              => Q_Sub(rename(x, scope), rename(y, scope), sub.pos)
        case gt@GreaterThan(x, y)       => Q_GreaterThan(rename(x, scope), rename(y, scope), gt.pos)
        case gte@GreaterThanEq(x, y)    => Q_GreaterThanEq(rename(x, scope), rename(y, scope), gte.pos)
        case lt@LessThan(x, y)          => Q_LessThan(rename(x, scope), rename(y, scope), lt.pos)
        case lte@LessThanEq(x, y)       => Q_LessThanEq(rename(x, scope), rename(y, scope), lte.pos)
        case eq@Eq(x, y)                => Q_Eq(rename(x, scope), rename(y, scope), eq.pos)
        case neq@NotEq(x, y)            => Q_NotEq(rename(x, scope), rename(y, scope), neq.pos)
        case and@And(x, y)              => Q_And(rename(x, scope), rename(y, scope), and.pos)
        case or@Or(x, y)                => Q_Or(rename(x, scope), rename(y, scope), or.pos)
        case not@Not(x)                 => Q_Not(rename(x, scope), not.pos)
        case neg@Neg(x)                 => Q_Neg(rename(x, scope), neg.pos)
        case l@Len(x)                   => Q_Len(rename(x, scope), l.pos)
        case o@Ord(x)                   => Q_Ord(rename(x, scope), o.pos)
        case c@Chr(x)                   => Q_Chr(rename(x, scope), c.pos)
        case il@IntLiteral(v)           => Q_IntLiteral(v, il.pos)
        case bl@BoolLiteral(v)          => Q_BoolLiteral(v, bl.pos)
        case cl@CharLiteral(v)          => Q_CharLiteral(v, cl.pos)
        case sl@StringLiteral(v)        => Q_StringLiteral(v, sl.pos)
        case arr@ArrayElem(v, indicies) => Q_ArrayElem(getName(v, scope), indicies.map(rename(_, scope)), arr.pos)
        case pairE@PairElem(index, v)   => Q_PairElem(index, rename(v, scope), pairE.pos)
        case id@Ident(v)                => rename(v, scope, id.pos)
        case PairNullLiteral            => Q_PairNullLiteral

    /**
      * Renames an ident.
      * @param ident the identity expression.
      * @param scope the scope we are in.
      * @return a qualified identity expression.
      */
    private def rename(ident: String, scope: Set[Name], pos: (Int, Int))(using ctx: RenamerContext): Q_Ident = 
        if (!scope.exists(_.value == ident)) then
            ctx.setPos(pos)
            ctx.errors += ScopeError(s"variable ${ident} not declared in scope") 
            throw ScopeException(ctx.errors.toList)

        Q_Ident(getName(ident, scope), pos)

    /**
      * Helper function that merges two scopes.
      * Takes the union whilst preserving the order.
      * @param scope1 the first scope.
      * @param scope2 the second scope.
      * @return the unions of both sets
      */
    private def merge(scope1: Set[Name], scope2: Set[Name]): Set[Name] = 
        val scope = MutableSet[Name]()

        for (name <- scope1) 
            scope += name
        
        for (name <- scope2) 
            if !scope.exists(_.value == name.value) then
                scope += name
        
        scope.toSet

    /**
      * Generates a new variable's name.
      * @param name the original name.
      * @param t the optional type.
      * @return the new name as a [[Name]]
      */
    private def newVar(name: String, t: Option[Type]): Name = 
        val _name = genName(name)
        varTypes(_name) = t match
            case Some(t) => toSemType(t)
            case None => ?

        _name
    
    /**
      * Adds a function's name to the global list of fnames.
      * @note functions are named first and they have to be unique 
           so each function will be called foo/0 bar/0 etc.
      * @param name the function's name.
      * @param t the return type of the function.
      * @param args the arguments as a list of names.
      */
    private def newFunc(name: Name, t: Type, args: List[Name]) =
        funcNames += name
        funcTypes(name) = (toSemType(t), args)

    /**
      * Sets the type of a variable to t.
      * @param name the variable.
      * @param t the new type.
      */
    private def updateType(name: Name, t: SemType) = 
        varTypes(name) = t

    /**
      * Generates a unique name for a variable by incrementing a counter.
      * @param name the string name of the variable as written in the program.
      */
    private def genName(name: String): Name = 
        val count = nameGenTable.getOrElse(name, 0)
        nameGenTable(name) = count + 1
        Name(name, count)

    /**
      * Retrieves the [[Name]] for a variable based on it's scope.
      * @param name the string name.
      * @param scope the current scope.
      * @return the respective [[Name]].
      */
    private def getName(name: String, scope: Set[Name]): Name = 
        if scope.exists(_.value == name) then
            scope.find(_.value == name).get
        else
            newVar(name, None)
    
    /**
      * Retrieves the [[Name]] for a function.
      * @param name the string name.
      * @return the respective [[Name]]. 
      */
    private def getFuncName(name: String)(using ctx: RenamerContext): Name = 
        if funcNames.exists(_.value == name) then
            funcNames.find(_.value == name).get 
        else
            ctx.errors += ScopeError(s"function $name not declared in scope")
            throw ScopeException(ctx.errors.toList)
    
    /**
      * Verifies that the function and variable types are known.
      * @param varTypes the map of variables to their types.
      * @param funcTypes the map of functions to their types. 
      * @return the var and func maps as [[KnownType]]s.
      */
    private def verifyTyped(varTypes: MutableMap[Name, SemType], funcTypes: MutableMap[Name, (SemType, List[Name])])(using ctx: RenamerContext): (Map[Name, KnownType], Map[Name, (KnownType, List[Name])]) = 
        val _varTypes = varTypes.map { (n, t) => (n, verifyTyped(t)) }.toMap
        val _funcTypes = funcTypes.map { (n, targs) => (n, (verifyTyped(targs._1), targs._2)) }.toMap

        (_varTypes, _funcTypes)
    
    /**
      * Verifies that a given type is known.
      * @param t the type in question.
      * @return the respective known type.
      */
    private def verifyTyped(t: SemType)(using ctx: RenamerContext): KnownType = t match
        case KnownType.Array(t) => KnownType.Array(verifyTyped(t))
        case ? => 
            ctx.errors += ScopeError("Variable not typed")
            throw ScopeException(ctx.errors.toList)
        
        case _ => t.asInstanceOf[KnownType]
}
