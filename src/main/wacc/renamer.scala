package wacc

import wacc.ast.*
import wacc.q_ast.*
import scala.collection.mutable.*

object renamer {
    private var globalScope: collection.mutable.Set[Q_Name] = collection.mutable.Set()

    private var name_gen_table: Map[String, Int] = Map[String, Int]()

    def rename(prog: Prog): Q_Prog = ???

    private def rename(funcs: List[Func]): List[Q_Func] = ???
    
    private def rename(func: Func): Q_Func = ???
    private def rename(param: Param): Q_Param = ???
    
    private def rename(stmts: List[Stmt], parScope: collection.immutable.Set[Q_Name], localScope: collection.immutable.Set[Q_Name]): List[Q_Stmt] = ???
    
    private def rename(stmt: Stmt, parScope: collection.immutable.Set[Q_Name], localScope: collection.immutable.Set[Q_Name]): Q_Stmt = ???

    private def rename(lvalue: LValue, scope: collection.immutable.Set[Q_Name]): Q_LValue = ???
    
    private def rename(rvalue: RValue, scope: collection.immutable.Set[Q_Name]): Q_RValue = ???

    private def rename(expr: Expr, scope: collection.immutable.Set[Q_Name]): Q_Expr = ???
}