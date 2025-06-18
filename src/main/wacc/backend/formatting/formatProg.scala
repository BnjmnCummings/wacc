package wacc.formatting

import wacc.assemblyIR.*

import java.io.Writer

/**
 * Formats the given program into assembly code and writes it to the provided writer.
 * @param prog The program to format.
 * @param writer The writer to write the formatted assembly code to.
 */
def formatProg(prog: A_Prog)(using writer: Writer) = 
    formatHeaders(prog.data)
    prog.funcs.foreach(formatFunction(_))

/**
 * Formats the headers of the given data section and writes it to the provided writer.
 * @param data The list of stored strings to format.
 * @param writer The writer to write the formatted headers to.
 */
def formatHeaders(data: List[A_StoredStr])(using writer: Writer) = 
    writer.write(".intel_syntax noprefix\n")
    writer.write(".globl main\n")
    writer.write(".section .rodata\n")
    data.foreach(formatData(_))
    writer.write(".text\n")

/**
 * Formats the given stored string and writes it to the provided writer.
 * @param storedStr The stored string to format.
 * @param writer The writer to write the formatted stored string to.
 */
def formatData(storedStr: A_StoredStr)(using writer: Writer) = 
    writer.write(s"\t.int ${storedStr.str.length()}\n")
    writer.write(s"${storedStr.lbl.name}:\n")
    writer.write(s"\t.asciz \"${storedStr.str}\"\n")

/**
 * Formats the given function or data function and writes it to the provided writer.
 * @param procedure The function or data function to format.
 * @param writer The writer to write the formatted function to.
 */
def formatFunction(procedure: A_SubRoutine)(using writer: Writer) = procedure match
    case A_Func(fLabel, instrs) => 
        writer.write(s"${fLabel.name}:\n")
        formatBody(instrs)

    case A_DataFunc(fLabel, instrs, data) =>
        writer.write(".section .rodata\n")
        writer.write(s"\t.int ${data.str.length()}\n")
        writer.write(s"${data.lbl.name}:\n")
        writer.write(s"\t.asciz \"${data.str}\"\n")
        writer.write(".text\n")
        writer.write(s"${fLabel.name}:\n")
        formatBody(instrs)

/**
 * Formats the given list of instructions and writes them to the provided writer.
 * @param instrs The list of instructions to format.
 * @param writer The writer to write the formatted instructions to.
 */
def formatBody(instrs: List[A_Instr])(using writer: Writer) =
    instrs.foreach(instr => instr match 
        case A_LabelStart(label) => writer.write(s"${label.name}:\n")
        case _ => 
            writer.write("\t")
            formatInstr(instr)
            writer.write("\n")
    )
