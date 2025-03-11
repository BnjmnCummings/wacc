package wacc.codeGen

import wacc.t_ast.* 
import wacc.TypeInfo
import wacc.q_ast.Name

import scala.collection.mutable

/**
  * Class to represent the stack tables of a function.
  * @param mainTable the main stack table of the function.
  */
class TableCtx(val mainTable: StackTables) {
    val funcTables: mutable.Map[Name, StackTables] = mutable.Map()
}

/**
  * Function to return the stack table context of a program.
  * Facilitates the first pass of the typed AST to pre-generate the stack tables for each scope.
  * @param prog the typed AST of the program.
  * @param typeInfo type information about functions and variables.
  * @return as stack table context.
  */
def getTables(prog: T_Prog, typeInfo: TypeInfo): TableCtx = 
    given ctx: TableCtx = TableCtx(StackTables(None, 0))
    ctx.mainTable.addScope(prog.scoped, typeInfo)

    getTables(prog.body, typeInfo, ctx.mainTable)

    prog.funcs.foreach { f => 
        val funcParamTable = StackTable(0)
        funcParamTable.addScope(f.args.map(_.v).toSet, typeInfo)

        val tables = StackTables(Some(funcParamTable), 0)
        tables.addScope(f.scoped, typeInfo)

        getTables(f.body, typeInfo, tables)
        ctx.funcTables += (f.v -> tables)
    }

    ctx

/**
  * Function to generate a stack table context from a list of instructions.
  * @param instrs list of instructions to get the stack tables of.
  * @param typeInfo type information about functions and variables.
  * @param target the stack tables to add the scope to.
  * @param ctx the table context.
  */
private def getTables(instrs: List[T_Stmt], typeInfo: TypeInfo, target: StackTables)(using ctx: TableCtx): Unit = 
    instrs.foreach { 
        getTables(_, typeInfo, target) 
    }

/**
 * Function to generate a stack table context for a nested scope.
 * Includes: If, While and Begin/End blocks.
 * @param instr instruction to get the stack tables of.
 * @param typeInfo type information about functions and variables.
 * @param target the stack tables to add the scope to.
 * @param ctx the table context.
 */
private def getTables(instr: T_Stmt, typeInfo: TypeInfo, target: StackTables)(using ctx: TableCtx): Unit = 
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
