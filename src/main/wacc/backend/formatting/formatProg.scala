package wacc.formatting

import wacc.assemblyIR.*
import java.io.Writer

def formatProg(prog: A_Prog)(using writer: Writer): Unit = 
    formatHeaders(prog.data)
    prog.funcs.foreach(formatFunction(_))

def formatHeaders(data: List[A_StoredStr])(using writer: Writer): Unit = 
    writer.write(".intel_syntax noprefix\n")
    writer.write(".globl main\n")
    writer.write(".section .rodata\n")
    data.foreach(formatData(_))
    writer.write(".text\n")

def formatData(storedStr: A_StoredStr)(using writer: Writer): Unit = 
    writer.write(s"\t.int ${storedStr.str.length()}\n")
    writer.write(s"${storedStr.lbl.name}:\n")
    writer.write(s"\t.asciz \"${storedStr.str}\"\n")

def formatFunction(procedure: A_Proc)(using writer: Writer): Unit = procedure match
    case A_Func(fLabel, instrs) => 
        writer.write(s"${fLabel.name}:\n")
        instrs.foreach(instr => writer.write("\t" + formatInstr(instr) + "\n"))

    case A_DataFunc(fLabel, instrs, data) =>
        writer.write(".section .rodata\n")
        writer.write(s"\t.int ${data.str.length()}\n")
        writer.write(s"${data.lbl.name}:\n")
        writer.write(s"\t.asciz \"${data.str}\"\n")
        writer.write(".text\n")
        writer.write(s"${fLabel.name}:\n")
        instrs.foreach(instr => writer.write("\t" + formatInstr(instr) + "\n"))
