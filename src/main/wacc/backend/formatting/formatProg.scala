package wacc.formatting

import wacc.assemblyIR.*

def formatProg(prog: A_Prog): String = 
    val sb = StringBuilder()
    sb ++= formatHeaders(prog.data)
    sb ++= prog.funcs.flatMap(formatFunction(_))
    sb.toString()

def formatHeaders(data: List[A_StoredStr]): String = 
    val sb = StringBuilder()
        sb ++= ".intel_syntax noprefix\n"
        sb ++= ".globl main\n"    
        sb ++= ".section .rodata\n"
        sb ++= data.flatMap(formatData(_))
        sb ++= ".text\n"
        sb.toString()

def formatData(storedStr: A_StoredStr): String = 
    val sb = StringBuilder()
        sb ++= s"\t.int ${storedStr.str.length()}\n"
        sb ++= s"${storedStr.lbl.name}:\n"
        sb ++= s"\t.asciz \"${storedStr.str}\"\n"
        sb.toString()

def formatFunction(procedure: A_Proc): String = procedure match
    case A_Func(fLabel, instrs) => 
        val sb = StringBuilder()
            sb ++= s"${fLabel.name}:\n"
            sb ++= instrs.flatMap("\t" + formatInstr(_) + "\n")
            sb.toString()

    case A_DataFunc(fLabel, instrs, data) =>
        val sb = StringBuilder()
            sb ++= ".section .rodata\n"
            sb ++= s"\t.int ${data.str.length()}\n"
            sb ++= s"${data.lbl.name}:\n"
            sb ++= s"\t.asciz \"${data.str}\"\n"
            sb ++= ".text\n"
            sb ++= s"${fLabel.name}:\n"
            sb ++= instrs.flatMap("\t" + formatInstr(_) + "\n")
            sb.toString()
