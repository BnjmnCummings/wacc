package wacc

import wacc.ast.*
import wacc.q_ast.*
import scala.collection.mutable.*

import collection.immutable.Set as Set
import collection.mutable.Set as MutableSet

object renamer {
    private var globalScope: MutableSet[Q_Name] = MutableSet()
    private var name_gen_table: Map[String, Int] = Map[String, Int]()

    def rename(prog: Prog): Q_Prog = 
        globalScope = MutableSet()
        name_gen_table = Map[String, Int]()

        // it is important this occurs first as it adds functions to globalScope
        val _funcs = rename(prog.funcs)
        val (_body, scoped) = rename(prog.body, Set(), Set())

        Q_Prog(_funcs, _body, scoped ++ globalScope)

    private def rename(funcs: List[Func]): List[Q_Func] = 
        val _funcs: ListBuffer[Q_Func] = ListBuffer()
        for (func <- funcs) {
            val _func = rename(func)
            globalScope += _func.v
            _funcs += _func
        }
        _funcs.toList

    private def rename(func: Func): Q_Func = 
        val localScope: MutableSet[Q_Name] = MutableSet()
        val _v: Q_Name = genName(func.v)
        val _args = func.args.map(rename)
            
        val (_body, scoped) = rename(func.body, _args.map(_.v).toSet, localScope.toSet)

        Q_Func(func.t, _v, _args, _body, scoped)
    
    
    private def rename(param: Param): Q_Param = 
        Q_Param(param.t, genName(param.v))
    
    private def rename(stmts: List[Stmt], parScope: Set[Q_Name], localScope: Set[Q_Name]): (List[Q_Stmt], Set[Q_Name]) = 
        val _localScope: MutableSet[Q_Name] = MutableSet()
        _localScope ++= localScope

        val _stmts: ListBuffer[Q_Stmt] = ListBuffer()
        for (stmt <- stmts) {
            val _stmt = rename(stmt, parScope, _localScope.toSet)
            _stmts += _stmt
            _stmt match {
                case Q_Decl(_, v, _) => {
                    if localScope.exists(_.old_name == v.old_name) then
                        throw ScopeException("Already declared in scope")
                    else
                        _localScope += v
                }
                case _ => ()
            }
        }
            
        (_stmts.toList, _localScope.toSet)
    
    
    private def rename(stmt: Stmt, parScope: Set[Q_Name], localScope: Set[Q_Name]): Q_Stmt = stmt match
        case Decl(t, Ident(v), r) => 
            if (localScope.exists(_.old_name == v)) {
                throw ScopeException("Already declared in scope")
            }
            /* need to evaluate r-value first so that we can't declare an ident as itself */
            val rvalue = rename(r, parScope ++ localScope)
            Q_Decl(t, genName(v), rvalue)
        
        case Asgn(l, r) => Q_Asgn(rename(l, parScope ++ localScope), rename(r, parScope ++ localScope))
        case Read(l) => Q_Read(rename(l, parScope ++ localScope))
        case Free(x) => Q_Free(rename(x, parScope ++ localScope))
        case Return(x) => Q_Return(rename(x, parScope ++ localScope))
        case Exit(x) => Q_Exit(rename(x, parScope ++ localScope))
        case Print(x) => Q_Print(rename(x, parScope ++ localScope))
        case Println(x) => Q_Println(rename(x, parScope ++ localScope))
        case If(cond, body, el) => 
            val (_body, scopedBody) = rename(body, parScope ++ localScope, Set())
            val (_el, scopedEl) = rename(el, parScope ++ localScope, Set())
            Q_If(rename(cond, parScope ++ localScope), _body, scopedBody, _el, scopedEl)
        case While(cond, body) => 
            val (_body, scoped) = rename(body, parScope ++ localScope, Set())
            Q_While(rename(cond, parScope ++ localScope), _body, scoped)
        case CodeBlock(body) =>
            val (_body, scoped) = rename(body, parScope ++ localScope, Set())
            Q_CodeBlock(_body, scoped)
        case Skip => Q_Skip

    private def rename(lvalue: LValue, scope: Set[Q_Name]): Q_LValue = lvalue match
        /* if the identity for an l-value doesn't yet exist, complain. */
        case Ident(v) => rename(v, scope)
        case ArrayElem(v, indicies) => {
            if (!scope.exists(_.old_name == v)) then{
                throw ScopeException(s"variable $v not declared in scope")
            }
            Q_ArrayElem(updateName(v, scope), indicies.map(rename(_, scope)))
        }
        /* recursively called on the contained l-value */
        case PairElem(index, v) => Q_PairElem(index, rename(v, scope))

    
    private def rename(rvalue: RValue, scope: Set[Q_Name]): Q_RValue = rvalue match
        case expr: Expr => rename(expr, scope)
        case FuncCall(v, args) => Q_FuncCall(updateName(v, scope), args.map(rename(_, scope)))
        case ArrayLiteral(xs) => Q_ArrayLiteral(xs.map(rename(_, scope)))
        case NewPair(x1, x2) => Q_NewPair(rename(x1, scope), rename(x2, scope))

    private def rename(expr: Expr, scope: Set[Q_Name]): Q_Expr = expr match
        case Mul(x, y) => Q_Mul(rename(x, scope), rename(y, scope))
        case Mod(x, y) => Q_Mod(rename(x, scope), rename(y, scope))
        case Add(x, y) => Q_Add(rename(x, scope), rename(y, scope))
        case Div(x, y) => Q_Div(rename(x, scope), rename(y, scope))
        case Sub(x, y) => Q_Sub(rename(x, scope), rename(y, scope))
        case GreaterThan(x, y) => Q_GreaterThan(rename(x, scope), rename(y, scope))
        case GreaterThanEq(x, y) => Q_GreaterThanEq(rename(x, scope), rename(y, scope))
        case LessThan(x, y) => Q_LessThan(rename(x, scope), rename(y, scope))
        case LessThanEq(x, y) => Q_LessThanEq(rename(x, scope), rename(y, scope))
        case Eq(x, y) => Q_Eq(rename(x, scope), rename(y, scope))
        case NotEq(x, y) => Q_NotEq(rename(x, scope), rename(y, scope))
        case And(x, y) => Q_And(rename(x, scope), rename(y, scope))
        case Or(x, y) => Q_Or(rename(x, scope), rename(y, scope))
        case Not(x) => Q_Not(rename(x, scope))
        case Neg(x) => Q_Neg(rename(x, scope))
        case Len(x) => Q_Len(rename(x, scope))
        case Ord(x) => Q_Ord(rename(x, scope))
        case Chr(x) => Q_Chr(rename(x, scope))
        case IntLiteral(v) => Q_IntLiteral(v)
        case BoolLiteral(v) => Q_BoolLiteral(v)
        case CharLiteral(v) => Q_CharLiteral(v)
        case StringLiteral(v) => Q_StringLiteral(v)
        case ArrayElem(v, indicies) => Q_ArrayElem(updateName(v, scope), indicies.map(rename(_, scope)))
        case PairElem(index, v) => Q_PairElem(index, rename(v, scope))
        case PairNullLiteral => Q_PairNullLiteral
        /* valid idents must be in scope */
        case Ident(v) => rename(v, scope)


    private def rename(ident: String, scope: Set[Q_Name]): Q_Ident = 
        if (!scope.exists(_.old_name == ident)) then 
            throw ScopeException(s"variable ${ident} not declared in scope")

        Q_Ident(updateName(ident, scope))
    
    private def genName(name: String): Q_Name = 
        val count = name_gen_table.getOrElse(name, 0)
        name_gen_table(name) = count + 1
        Q_Name(name, s"${name}/$count")

    private def updateName(name: String, localScope: Set[Q_Name]): Q_Name = 
        if localScope.exists(_.old_name == name) then
            localScope.find(_.old_name == name).get
        else if globalScope.exists(_.old_name == name) then
            globalScope.find(_.old_name == name).get 
        else
            genName(name)
}
