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
    sb ++= ".global main\n"    
    sb ++= ".section .rodata\n"
    sb ++= data.flatMap(formatData(_))
    sb ++= ".text\n"
    sb.toString()

def formatData(storedStr: A_StoredStr): String = 
    val sb = StringBuilder()
    sb ++= s"\t.int ${storedStr.str.length()}\n"
    //TODO: verify that storedStr.lbl contains the number :)
    sb ++= s"${storedStr.lbl.name}:\n"
    sb ++= s"\t.asciz: \"${storedStr.str}\"\n"
    sb.toString()

def formatFunction(func: A_Func): String = 
    val sb = StringBuilder()
    /* any reason why label can't just be a string? */
    sb ++= s"${func.lbl.name}:"
    sb.toString()

