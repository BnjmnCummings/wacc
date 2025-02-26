package wacc.formatting

import wacc.assemblyIR.*
import java.io.Writer

def formatProg(prog: A_Prog)(using writer: Writer) = 
    formatHeaders(prog.data)
    prog.funcs.foreach(formatFunction(_))

def formatHeaders(data: List[A_StoredStr])(using writer: Writer) = 
    writer.write(".intel_syntax noprefix\n")
    writer.write(".globl main\n")
    writer.write(".section .rodata\n")
    data.foreach(formatData(_))
    writer.write(".text\n")

def formatData(storedStr: A_StoredStr)(using writer: Writer) = 
    writer.write(s"\t.int ${storedStr.str.length()}\n")
    writer.write(s"${storedStr.lbl.name}:\n")
    writer.write(s"\t.asciz \"${storedStr.str}\"\n")

def formatFunction(procedure: A_Proc)(using writer: Writer) = procedure match
    case A_Func(fLabel, instrs) => 
        writer.write(s"${fLabel.name}:\n")
        formatBodyInstr(instrs)

    case A_DataFunc(fLabel, instrs, data) =>
        writer.write(".section .rodata\n")
        writer.write(s"\t.int ${data.str.length()}\n")
        writer.write(s"${data.lbl.name}:\n")
        writer.write(s"\t.asciz \"${data.str}\"\n")
        writer.write(".text\n")
        writer.write(s"${fLabel.name}:\n")
        formatBodyInstr(instrs)

def formatBodyInstr(instrs: List[A_Instr])(using writer: Writer) =
    instrs.foreach(instr => instr match 
        case A_LabelStart(label) => writer.write(s"${label.name}:\n")
        case _ => writer.write("\t" + formatInstr(instr) + "\n")
    )
    

