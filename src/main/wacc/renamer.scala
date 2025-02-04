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

    private def rename(funcs: List[Func]): List[Q_Func] = ???
    
    private def rename(func: Func): Q_Func = ???
    
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