package wacc

import wacc.ast.*
import wacc.q_ast.*
import scala.collection.mutable.*

object renamer {
    private var globalScope: collection.mutable.Set[Q_Name] = collection.mutable.Set()

    private var name_gen_table: Map[String, Int] = Map[String, Int]()

    def rename(prog: Prog): Q_Prog = prog match
        case Prog(funcs, body) => {
            globalScope = collection.mutable.Set()
            name_gen_table = Map[String, Int]()

            // it is important this occurs first as it adds functions to globalScope
            val _funcs = rename(funcs)

            Q_Prog(_funcs, rename(body, collection.immutable.Set(), collection.immutable.Set()))
        }

    private def rename(funcs: List[Func]): List[Q_Func] =
        val _funcs: ListBuffer[Q_Func] = ListBuffer()
        for func <- funcs do
            val _func = rename(func)
            globalScope += _func.v
            _funcs += _func
        _funcs.toList
    
    private def rename(func: Func): Q_Func = func match
        case Func(t, v, args, body) => {
            val localScope: collection.mutable.Set[Q_Name] = collection.mutable.Set()

            val _v: Q_Name = genName(v)

            val _args = args.map(rename)
            localScope ++= _args.map(_.v)

            Q_Func(t, _v, _args, rename(body, collection.immutable.Set(), localScope.toSet))
        }
    
    private def rename(param: Param): Q_Param = ???
    
    private def rename(stmts: List[Stmt], parScope: collection.immutable.Set[Q_Name], localScope: collection.immutable.Set[Q_Name]): List[Q_Stmt] = ???
    
    private def rename(stmt: Stmt, parScope: collection.immutable.Set[Q_Name], localScope: collection.immutable.Set[Q_Name]): Q_Stmt = ???

    private def rename(lvalue: LValue, scope: collection.immutable.Set[Q_Name]): Q_LValue = ???
    
    private def rename(rvalue: RValue, scope: collection.immutable.Set[Q_Name]): Q_RValue = ???

    private def rename(expr: Expr, scope: collection.immutable.Set[Q_Name]): Q_Expr = ???
    
    
    private def genName(name: String): Q_Name = {
        val count = name_gen_table.getOrElse(name, 0)
        name_gen_table(name) = count + 1
        Q_Name(name, s"${name}/$count")
    }

    private def updateName(name: String, localScope: collection.immutable.Set[Q_Name]): Q_Name = {
        if localScope.exists(_.old_name == name) then
            localScope.find(_.old_name == name).get
        else if globalScope.exists(_.old_name == name) then
            globalScope.find(_.old_name == name).get 
        else
            genName(name)
    }
}