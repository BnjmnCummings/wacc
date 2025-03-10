package wacc.codeGen

import wacc.t_ast.* 
import wacc.TypeInfo
import wacc.q_ast.Name

import scala.collection.mutable

class TableCtx(val mainTable: StackTables) {
    val funcTables: mutable.Map[Name, StackTables] = mutable.Map()
}

def getTables(t: T_Prog, typeInfo: TypeInfo): TableCtx = 
    given ctx: TableCtx = TableCtx(StackTables(None, 0))
    ctx.mainTable.addScope(t.scoped, typeInfo)

    getTables(t.body, typeInfo, ctx.mainTable)

    t.funcs.foreach(f => {
        val funcParamTable = StackTable(0)
        funcParamTable.addScope(f.args.map(_.v).toSet, typeInfo)

        val tables = StackTables(Some(funcParamTable), 0)
        
        tables.addScope(f.scoped, typeInfo)

        getTables(f.body, typeInfo, tables)
        
        ctx.funcTables += (f.v -> tables)
    })

    ctx

def getTables(instrs: List[T_Stmt], typeInfo: TypeInfo, target: StackTables)(using ctx: TableCtx): Unit = 
    instrs.foreach { 
        getTables(_, typeInfo, target) 
    }

def getTables(instr: T_Stmt, typeInfo: TypeInfo, target: StackTables)(using ctx: TableCtx): Unit = 
    instr match 
        case T_If(cond, body, scopedBody, el, scopedEl) =>
            target.addScope(scopedBody, typeInfo)
            getTables(body, typeInfo, target)
            target.addScope(scopedEl, typeInfo)
            getTables(el, typeInfo, target)

        case T_While(cond, body, scoped) =>
            target.addScope(scoped, typeInfo)
            getTables(body, typeInfo, target)

        case T_CodeBlock(body, scoped) =>
            target.addScope(scoped, typeInfo)
            getTables(body, typeInfo, target)

        case _ => ()
