package wacc.integration

import wacc.testUtils.*
import wacc.frontend
import wacc.assemblyIR.A_Prog
import wacc.codeGen.*
import wacc.formatting.*

import java.io.FileNotFoundException
import java.io.File
import java.io.ByteArrayInputStream as ByteArrayIn
import org.scalatest.flatspec.AnyFlatSpec
import sys.process._
import scala.io.Source
import scala.collection.mutable.ListBuffer

class backend_integration_test extends ConditionalRun {

    runIfTrue(testSettings, "integration_exit", () => {
        "exit" should "successfully produce the desired output and exit code" in {
            val paths: List[String] = searchDir(File("wacc-examples/valid/basic/exit"))
            runTests(paths)
        }   
    })

    runIfTrue(testSettings, "integration_skip", () => {
        "skip" should "successfully produce the desired output and exit code" in {
            val paths: List[String] = searchDir(File("wacc-examples/valid/basic/skip"))
            runTests(paths)
        }   
    })

    runIfTrue(testSettings, "integration_advanced", () => {
        "advanced" should "successfully produce the desired output and exit code" in {
            val paths: List[String] = searchDir(File("wacc-examples/valid/advanced"))
            runTests(paths)
        }   
    })

    runIfTrue(testSettings, "integration_array", () => {
        "array" should "successfully produce the desired output and exit code" in {
            val paths: List[String] = searchDir(File("wacc-examples/valid/array"))
            runTests(paths)
        }   
    })

    runIfTrue(testSettings, "integration_expressions", () => {
        "expressions" should "successfully produce the desired output and exit code" in {
            val paths: List[String] = searchDir(File("wacc-examples/valid/expressions"))
            runTests(paths)
        }   
    })

    runIfTrue(testSettings, "integration_nested_functions", () => {
        "nested_functions" should "successfully produce the desired output and exit code" in {
            val paths: List[String] = searchDir(File("wacc-examples/valid/function/nested_functions"))
            runTests(paths)
        }   
    })

    runIfTrue(testSettings, "integration_simple_functions", () => {
        "simple_functions" should "successfully produce the desired output and exit code" in {
            val paths: List[String] = searchDir(File("wacc-examples/valid/function/simple_functions"))
            runTests(paths)
        }   
    })

    runIfTrue(testSettings, "integration_if", () => {
        "if" should "successfully produce the desired output and exit code" in {
            val paths: List[String] = searchDir(File("wacc-examples/valid/if"))
            runTests(paths)
        }   
    })

    runIfTrue(testSettings, "integration_IO_all", () => {
        "IO_all" should "successfully produce the desired output and exit code" in {
            val paths: List[String] = searchDir(File("wacc-examples/valid/IO"))
            runTests(paths)
        }   
    })

    runIfTrue(testSettings, "integration_IO_print", () => {
        "IO_print" should "successfully produce the desired output and exit code" in {
            val paths: List[String] = searchDir(File("wacc-examples/valid/IO/print"))
            runTests(paths)
        }   
    })

    runIfTrue(testSettings, "integration_IO_read", () => {
        "IO_read" should "successfully produce the desired output and exit code" in {
            val paths: List[String] = searchDir(File("wacc-examples/valid/IO/read"))
            runTests(paths)
        }   
    })

    runIfTrue(testSettings, "integration_pairs", () => {
        "pairs" should "successfully produce the desired output and exit code" in {
            val paths: List[String] = searchDir(File("wacc-examples/valid/pairs"))
            runTests(paths)
        }   
    })

    runIfTrue(testSettings, "integration_runtimeErr_arrayOutOfBounds", () => {
        "runtimeErr_arrayOutOfBounds" should "successfully produce the desired output and exit code" in {
            val paths: List[String] = searchDir(File("wacc-examples/valid/runtimeErr/arrayOutOfBounds"))
            runTests(paths)
        }   
    })

    runIfTrue(testSettings, "integration_runtimeErr_badChar", () => {
        "runtimeErr_badChar" should "successfully produce the desired output and exit code" in {
            val paths: List[String] = searchDir(File("wacc-examples/valid/runtimeErr/badChar"))
            runTests(paths)
        }   
    })

    runIfTrue(testSettings, "integration_runtimeErr_divideByZero", () => {
        "runtimeErr_divideByZero" should "successfully produce the desired output and exit code" in {
            val paths: List[String] = searchDir(File("wacc-examples/valid/runtimeErr/divideByZero"))
            runTests(paths)
        }   
    })

    runIfTrue(testSettings, "integration_runtimeErr_integerOverflow", () => {
        "runtimeErr_integerOverflow" should "successfully produce the desired output and exit code" in {
            val paths: List[String] = searchDir(File("wacc-examples/valid/runtimeErr/integerOverflow"))
            runTests(paths)
        }   
    })

    runIfTrue(testSettings, "integration_runtimeErr_nullDereference", () => {
        "runtimeErr_nullDereference" should "successfully produce the desired output and exit code" in {
            val paths: List[String] = searchDir(File("wacc-examples/valid/runtimeErr/nullDereference"))
            runTests(paths)
        }   
    })

    runIfTrue(testSettings, "integration_scope", () => {
        "scope" should "successfully produce the desired output and exit code" in {
            val paths: List[String] = searchDir(File("wacc-examples/valid/scope"))
            runTests(paths)
        }   
    })

    runIfTrue(testSettings, "integration_sequence", () => {
        "sequence" should "successfully produce the desired output and exit code" in {
            val paths: List[String] = searchDir(File("wacc-examples/valid/sequence"))
            runTests(paths)
        }   
    })

    runIfTrue(testSettings, "integration_variables", () => {
        "variables" should "successfully produce the desired output and exit code" in {
            val paths: List[String] = searchDir(File("wacc-examples/valid/variables"))
            runTests(paths)
        }   
    })

    runIfTrue(testSettings, "integration_while", () => {
        "while" should "successfully produce the desired output and exit code" in {
            val paths: List[String] = searchDir(File("wacc-examples/valid/while"))
            runTests(paths)
        }   
    })

    /**
     * Runs a selected batch of tests by generating the assembly and comparing output
     * @param paths the list of file paths to .wacc files we want to test
     */ 
    def runTests(paths: List[String]) = {
        val successes: ListBuffer[String] = ListBuffer.empty[String]
        val outputFailures: ListBuffer[String] = ListBuffer.empty[String]
        val compileFailures: ListBuffer[String] = ListBuffer.empty[String]
        val synFailures: ListBuffer[String] = ListBuffer.empty[String]
        val semFailures: ListBuffer[String] = ListBuffer.empty[String]

        
        paths.foreach {filePath => 
            val (tProg, typeInfo) = frontend(filePath)
            try {
                val assembly: A_Prog = gen(tProg, typeInfo)
                val progName = filePath
                .split("/")
                .last
                .replace(".wacc", "")
                
                val printWriter = new java.io.PrintWriter(s"src/test/wacc/backend/integration/assembly/$progName.s")
                formatProg(assembly)(using printWriter)
                printWriter.close()
                
                val expected@(expExitCode, expOutput, input) = getExpectedOutput(filePath)
                val actual@(actualExitCode, actualOutput) = runAssembly(progName, input)

                if ( actualExitCode == expExitCode && actualOutput == expOutput)
                    successes += filePath
                    s"./wipeAss.sh $progName" .!
                else 
                    outputFailures += filePath
                    if (input.nonEmpty) {
                        info(s"input: ${input.mkString(" ")}")
                    }
                    info(s"expected: $expected")
                    info(s"actual: $actual")
                    info("naughty boy")

            } catch {
                case e: NotImplementedError => 
                    compileFailures += filePath
                case e: InstantiationException => 
                    compileFailures += filePath
                case e: Exception => 
                    compileFailures += filePath
                    println(e)
                    println(e.getStackTrace().mkString("\n"))
            }
        }

        val successList: List[String] = successes.toList
        val synFailList: List[String] = synFailures.toList
        val semFailList: List[String] = semFailures.toList
        val outputFailList: List[String] = outputFailures.toList
        val compileFailList: List[String] = compileFailures.toList

        /* report successful test cases */
        info("correctly succeeding tests:\n")
        successList.map(s => s.split("valid/").last).foreach(info(_))

        /* report failing test cases */
        if (!synFailList.isEmpty) {
            fail(
                "some of the paths failed to parse (they are syntactically valid and should succeed):\n\n" 
                + synFailList.map(s => s.split("syntaxErr/").last).mkString("\n")
            )
        }
        if (!semFailList.isEmpty) {
            fail(
                "some of the paths failed to scope/type check (they are semantically valid and should succeed):\n\n" 
                + semFailList.map(s => s.split("semanticErr/").last).mkString("\n")
            )
        }
        if (!outputFailList.isEmpty) {
            fail(
                "some of the paths failed (assembly output did not mach expected output):\n\n" 
                + outputFailList.map(s => s.split("valid/").last).mkString("\n")
            )
        }
        if (!compileFailList.isEmpty) {
            fail(
                "some of the paths failed (assembly did not compile):\n\n" 
                + compileFailList.map(s => s.split("valid/").last).mkString("\n")
            )
        }
    }

    /**
    * 
    * @param progName the path to the gend 'progName.s' assembly file
    * @return a pair: (exit code, output)
    */
    def runAssembly(progName: String, input: String): (Int, List[String]) = {
        val fileName = "assembly/" + progName
        val buildExitStatus = s"./buildAss.sh $fileName" .!

        if (buildExitStatus == 0) {
            val output: ListBuffer[String] = ListBuffer()
            val cmd = s"./src/test/wacc/backend/integration/$fileName" #< new ByteArrayIn(input.getBytes)
            val process = cmd.run( 
                ProcessLogger(
                    // fout
                    line => output += line,
                    // ferr
                    line => output += line
                )
            )
            val exitStatus = process.exitValue()

            /* clean up after ourselves and return */
            s"./wipeObj.sh $fileName" .!

            /* filter output */
            val cleanOutput = output.toList
                .filter(_.nonEmpty)
                .map(_.replaceAll("0x[0-9a-fA-F]+", "#addrs#"))
                .map(line =>  
                    if line.contains("rror:")then
                        "#runtime_error#"
                    else 
                        line
                )
            return (exitStatus, cleanOutput)

        } else {
            s"./wipeObj.sh $fileName" .!
            throw InstantiationException(s"Build command failed with exit status: $buildExitStatus")
        }
    }

    /**
     * @param fileName the path to the .wacc file
     * @return an expected pair: (exit code, output)
     */
    def getExpectedOutput(fileName: String): (Int, List[String], String) =
        try {
            val lines = Source.fromFile(fileName).getLines().toList
            val input = lines.filter(_.startsWith("# Input: ")) match {
                case Nil => ""
                case (headInput::_) => headInput.replace("# Input: ", "")
            }
            
            val output = lines
                .dropWhile( _ != "# Output:").tail
                .takeWhile(s => s != "# Program:" && s != "# Exit:")
                .filter(_.startsWith("# "))
                .map(_.drop(2))
                .filter(_.nonEmpty)
            
            /* 'Exit:' comment isn't always present */
            val exitCode: Int = lines.dropWhile( _ != "# Exit:") match
                case Nil => 0
                case _::tail => 
                    tail.takeWhile(_ != "# Program:")
                    .map(_.replace("#", "").trim)
                    .filter(_.nonEmpty)
                    .map(_.toInt)(0)

            return (exitCode, output, input) 
        } catch {
            case e: FileNotFoundException => {
                throw FileNotFoundException(s"File Not Found: $fileName")
            } 
        }
}
