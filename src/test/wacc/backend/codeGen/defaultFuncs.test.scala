package wacc.codeGen

import wacc.testUtils.*
import wacc.assemblyIR.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

class default_function_test extends AnyFlatSpec, ConditionalRun {

    runIfTrue(testSettings, "default_function_test", () => {
        "defaultExit" should "generate correct assembly instructions" in {
            val func = defaultExit
            func.lbl shouldBe EXIT_LABEL
            func.instrs should not be empty
        }

        "defaultOverflow" should "generate correct assembly instructions" in {
            val func = defaultOverflow
            func.lbl shouldBe ERR_OVERFLOW_LABEL
            func.instrs should not be empty
        }

        "defaultPrintln" should "generate correct assembly instructions" in {
            val func = defaultPrintln
            func.lbl shouldBe PRINTLN_LABEL
            func.instrs should not be empty
        }

        "defaultPrinti" should "generate correct assembly instructions" in {
            val func = defaultPrinti
            func.lbl shouldBe PRINTI_LABEL
            func.instrs should not be empty
        }

        "defaultPrintc" should "generate correct assembly instructions" in {
            val func = defaultPrintc
            func.lbl shouldBe PRINTC_LABEL
            func.instrs should not be empty
        }

        "defaultPrintp" should "generate correct assembly instructions" in {
            val func = defaultPrintp
            func.lbl shouldBe PRINTP_LABEL
            func.instrs should not be empty
        }

        "defaultPrintb" should "generate correct assembly instructions" in {
            val func = defaultPrintb
            func.lbl shouldBe PRINTB_LABEL
            func.instrs should not be empty
        }

        "defaultPrints" should "generate correct assembly instructions" in {
            val func = defaultPrints
            func.lbl shouldBe PRINTS_LABEL
            func.instrs should not be empty
        }

        "defaultReadc" should "generate correct assembly instructions" in {
            val func = defaultReadc
            func.lbl shouldBe READC_LABEL
            func.instrs should not be empty
        }

        "defaultReadi" should "generate correct assembly instructions" in {
            val func = defaultReadi
            func.lbl shouldBe READI_LABEL
            func.instrs should not be empty
        }

        "defaultBadChar" should "generate correct assembly instructions" in {
            val func = defaultBadChar
            func.lbl shouldBe ERR_BAD_CHAR_LABEL
            func.instrs should not be empty
        }

        "defaultDivZero" should "generate correct assembly instructions" in {
            val func = defaultDivZero
            func.lbl shouldBe ERR_DIV_ZERO_LABEL
            func.instrs should not be empty
        }

        "defaultOutOfBounds" should "generate correct assembly instructions" in {
            val func = defaultOutOfBounds
            func.lbl shouldBe ERR_OUT_OF_BOUNDS_LABEL
            func.instrs should not be empty
        }

        "defaultOutOfMemory" should "generate correct assembly instructions" in {
            val func = defaultOutOfMemory
            func.lbl shouldBe ERR_OUT_OF_MEMORY_LABEL
            func.instrs should not be empty
        }

        "defaultMalloc" should "generate correct assembly instructions" in {
            val func = defaultMalloc
            func.lbl shouldBe MALLOC_LABEL
            func.instrs should not be empty
        }

        "defaultFree" should "generate correct assembly instructions" in {
            val func = defaultFree
            func.lbl shouldBe FREE_LABEL
            func.instrs should not be empty
        }

        "defaultFreePair" should "generate correct assembly instructions" in {
            val func = defaultFreePair
            func.lbl shouldBe FREE_PAIR_LABEL
            func.instrs should not be empty
        }

        "defaultErrNull" should "generate correct assembly instructions" in {
            val func = defaultErrNull
            func.lbl shouldBe ERR_NULL_PAIR_LABEL
            func.instrs should not be empty
        }
    })
}
