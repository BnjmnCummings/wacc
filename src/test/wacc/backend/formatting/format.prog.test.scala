package wacc.formatting

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import java.io.StringWriter

import wacc.testUtils.*

import wacc.assemblyIR.*
import wacc.assemblyIR.A_OperandSize

class format_prog_test extends ConditionalRun {
    runIfTrue(testSettings, "format_prog_test", () => {
        "formatProg" should "format a program with no data and no functions" in {
            val writer = new StringWriter()
            val prog = A_Prog(List(), List())
            formatProg(prog)(using writer)
            val progData = writer.toString()
            progData shouldBe ".intel_syntax noprefix\n" +
              ".globl main\n." +
              "section .rodata\n" +
              ".text\n"
        }

        it should "format a program with no data and one function" in {
            val writer = new StringWriter()
            val func = A_Func(A_InstrLabel("main"), List())
            val prog = A_Prog(List(), List(func))
            formatProg(prog)(using writer)
            val progData = writer.toString()
            progData shouldBe ".intel_syntax noprefix\n" +
              ".globl main\n" +
              ".section .rodata\n" +
              ".text\n" +
              "main:\n"
        }

        it should "format a program with data and functions" in {
            val writer = new StringWriter()
            val data = List(A_StoredStr(A_DataLabel("str"), "hello world!"))
            val func = A_Func(A_InstrLabel("main"), List())
            val func2 = A_Func(A_InstrLabel("main2"), List())
            val prog = A_Prog(data, List(func, func2))
            formatProg(prog)(using writer)
            val progData = writer.toString()
            progData shouldBe ".intel_syntax noprefix\n" +
              ".globl main\n" +
              ".section .rodata\n" +
              "\t.int 12\n" +
              "str:\n" +
              "\t.asciz \"hello world!\"\n" +
              ".text\n" +
              "main:\n" +
              "main2:\n"
        }
    })
}

class format_headers_test extends ConditionalRun {
    runIfTrue(testSettings, "format_headers_test", () => {
        "formatHeaders" should "format headers for a program with no data" in {
            val writer = new StringWriter()
            formatHeaders(List())(using writer)
            val headers = writer.toString()
            headers shouldBe ".intel_syntax noprefix\n" +
              ".globl main\n." +
              "section .rodata\n" +
              ".text\n"
        }

        it should "format headers for a program with one stored string" in {
            val writer = new StringWriter()
            val data = List(A_StoredStr(A_DataLabel("str"), "hello world!"))
            formatHeaders(data)(using writer)
            val headers = writer.toString()
            headers shouldBe ".intel_syntax noprefix\n" +
              ".globl main\n" +
              ".section .rodata\n" +
              "\t.int 12\n" +
              "str:\n" +
              "\t.asciz \"hello world!\"\n" +
              ".text\n"
        }

        it should "format headers for a program with multiple stored strings" in {
            val writer = new StringWriter()
            val data = List(
                A_StoredStr(A_DataLabel("str1"), "hello world!"),
                A_StoredStr(A_DataLabel("str2"), "hello!")
            )
            formatHeaders(data)(using writer)
            val headers = writer.toString()
            headers shouldBe ".intel_syntax noprefix\n" +
              ".globl main\n" +
              ".section .rodata\n" +
              "\t.int 12\n" +
              "str1:\n" +
              "\t.asciz \"hello world!\"\n" +
              "\t.int 6\n" +
              "str2:\n" +
              "\t.asciz \"hello!\"\n" +
              ".text\n"
        }
    })
}

class format_data_test extends ConditionalRun {
    runIfTrue(testSettings, "format_data_test", () => {
        "formatData" should "format a stored string" in {
            val writer = new StringWriter()
            val storedStr = A_StoredStr(A_DataLabel("str"), "hello world!")
            formatData(storedStr)(using writer)
            val data = writer.toString()
            data shouldBe "\t.int 12\nstr:\n\t.asciz \"hello world!\"\n"
        }

        it should "format an empty string" in {
            val writer = new StringWriter()
            val storedStr = A_StoredStr(A_DataLabel("str"), "")
            formatData(storedStr)(using writer)
            val data = writer.toString()
            data shouldBe "\t.int 0\nstr:\n\t.asciz \"\"\n"
        }
    })
}

class format_function_test extends ConditionalRun {
    runIfTrue(testSettings, "format_function_test", () => {
        "formatFunction" should "format an empty function" in {
            val writer = new StringWriter()
            val func = A_Func(A_InstrLabel("main"), List())
            formatFunction(func)(using writer)
            val funcData = writer.toString()
            funcData shouldBe "main:\n"
        }

        it should "format a function with one instruction" in {
            val writer = new StringWriter()
            val func = A_Func(A_InstrLabel("main"), List(A_Ret))
            formatFunction(func)(using writer)
            val funcData = writer.toString()
            funcData shouldBe "main:\n\tret\n"
        }

        it should "format a function with multiple instructions" in {
            val writer = new StringWriter()
            val func = A_Func(
                A_InstrLabel("main"), 
                List(A_Add(A_Reg(A_RegName.RetReg), A_Imm(3), PTR_SIZE), A_Ret)
            )
            formatFunction(func)(using writer)
            val funcData = writer.toString()
            funcData shouldBe "main:\n\tadd rax, 3\n\tret\n"
        }
    })
}
