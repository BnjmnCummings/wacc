package wacc.integration

import wacc.testUtils.*
import wacc.frontend
import wacc.assemblyIR.A_Prog
import wacc.codeGen.*
import wacc.formatting.*

import java.io.FileNotFoundException
import java.io.File
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

    def runTests(paths: List[String]) = {
        //runAssembly("andExpr")
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
                
                val (expExitCode, expOutput, input) = getExpectedOutput(filePath)
                val actual = runAssembly(progName, input)

                if(actual._1 == expExitCode && actual._2.zip(expOutput).forall{_ match 
                    case (a, "#runtime_error#") => a.contains("fatal error") || a.contains("Error: ")
                    case (a, "Printing an array variable gives an address, such as #addrs#") => a.contains("Printing an array variable gives an address, such as 0x")
                    case (a, "#addrs# = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}") => a.contains("0x") && a.contains(" = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}")
                    case (a, e) => a == e
                })
                                                                                    
                    successes += filePath
                    s"./wipeAss $progName" .!
                else 
                    outputFailures += filePath
                    if (input.nonEmpty) {
                        info(s"input: ${input.mkString(" ")}")
                    }
                    info(s"expected: $expOutput")
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
        /* TODO: refactor with regex*/
        val fileName = "assembly/" + progName
        val buildExitStatus = s"./buildAss $fileName" .!

        if (buildExitStatus == 0) {
            val cmd = s"./src/test/wacc/backend/integration/$fileName"

            val output: ListBuffer[String] = ListBuffer()

            val exitStatus = cmd.run(ProcessIO(
                stdin => {
                    if (input.nonEmpty) {
                        stdin.write(input.getBytes)
                        stdin.close()
                    }
                },
                stdout => scala.io.Source
                            .fromInputStream(stdout)
                            .getLines
                            .foreach(output += _),
                _ => ()
            )).exitValue()
            
            /* clean up after ourselves and return */
            s"./wipeObj $fileName" .!

            return (exitStatus, output.toList.filter(_.nonEmpty))

        } else {
            s"./wipeObj $fileName" .!
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
            val input = {
                val filtered = lines.filter(_.startsWith("# Input:"))
                if filtered.length == 0 then ""
                else filtered(0).drop(8)
            }
            val output = lines
                .dropWhile( _ != "# Output:").tail
                .takeWhile(s => s != "# Program:" && s != "# Exit:")
                // filter so it only takes lines of the format "# .*" and removes the "# "
                .filter(_.startsWith("# "))
                .map(_.drop(2))
                .filter(_.nonEmpty)
            
            val exitCode: Int = lines.dropWhile( _ != "# Exit:") match
                /* 'Exit:' comment isn't always present */
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
    
    // /** 
    //  * Helper function to collect all the filepaths to valid wacc programs
    //  * TODO: change back to all wacc programs
    //  */ 
    // def getFilePaths(fPathStart: String): List[String] = {
    //     //searchDir(File(fPathStart))
    //     List("wacc-examples/valid/expressions/andExpr.wacc")
    // }
}
