package wacc.integration

import wacc.semantic.typeCheck
import wacc.renamer
import wacc.parser
import wacc.ScopeException

import java.io.FileNotFoundException
import java.io.File
import parsley.Success
import org.scalatest.flatspec.AnyFlatSpec
import sys.process._
import scala.io.Source
import scala.collection.mutable.ListBuffer

class backend_integration_test extends AnyFlatSpec {
    
    "backend" should "successfully produce the desired output and exit code" in {
        runAssembly("andExpr")
        val successes: ListBuffer[String] = ListBuffer.empty[String]
        val backendFailures: ListBuffer[String] = ListBuffer.empty[String]
        val synFailures: ListBuffer[String] = ListBuffer.empty[String]
        val semFailures: ListBuffer[String] = ListBuffer.empty[String]

        getFilePaths().foreach { filePath => parser.parseF(File(filePath)) match 
            case Success(t) => {
                try {
                    /* front end pipeline */
                    val (q_t, tyInfo) = renamer.rename(t)
                    typeCheck(q_t, tyInfo) match {
                        case Right(t_prog) =>
                            // TODO: generateAssembly(t_prog)
                            val progName = filePath
                                .split("/")
                                .last
                                .replace(".wacc","")

                            if(runAssembly(progName) == getExpectedOutput(filePath))
                                successes += filePath
                            else 
                                backendFailures += filePath
                        
                        case _ => semFailures += filePath
                    }

                } catch {
                    case e: ScopeException => semFailures += filePath
                }
            }
            case _ => synFailures += filePath
        }

        val successList: List[String] = successes.toList
        val synFailList: List[String] = synFailures.toList
        val semFailList: List[String] = semFailures.toList
        val backendFailList: List[String] = backendFailures.toList

        /* report successful test cases */
        info("correctly succeeding tests:\n")
        successList.map(s => s.split("valid/").last).foreach(info(_))

        if (synFailList.length != 0) {
            fail(
                "some of the paths failed to parse (they are syntactically valid and should succeed):\n\n" 
                + synFailList.map(s => s.split("syntaxErr/").last).mkString("\n")
            )
        }
        if (semFailList.length != 0) {
            fail(
                "some of the paths failed to scope/type check (they are semantically valid and should succeed):\n\n" 
                + semFailList.map(s => s.split("semanticErr/").last).mkString("\n")
            )
        }
        /* report failing test cases */
        if (backendFailList.length != 0) {
            fail(
                "some of the paths failed (they are valid and should succeed):\n\n" 
                + backendFailList.map(s => s.split("valid/").last).mkString("\n")
            )
        }
    } 

    /**
    * 
    * @param progName the path to the generated 'progName.s' assembly file
    * @return a pair: (exit code, output)
    */
    def runAssembly(progName: String): (Int, List[String]) = {
        /* TODO: refactor with regex*/
        val fileName = "assembly/" + progName
        val buildExitStatus = s"./buildAss $fileName" .!

        if (buildExitStatus == 0) {
            val cmd = s"./src/test/wacc/backend/integration/$fileName"
            val exitStatus = cmd .!
            val output: ListBuffer[String] = ListBuffer.empty[String]

            if(exitStatus == 0) {
                output ++= (cmd .!!).split('\n')
            }

            /* clean up after ourselves and return */
            s"./wipeAss $fileName" .!
            return (exitStatus, output.toList)

        } else {
            throw InstantiationException(s"Build command failed with exit status: $buildExitStatus")
        }
    }

    /**
     * @param fileName the path to the .wacc file
     * @return an expected pair: (exit code, output)
     */
    def getExpectedOutput(fileName: String): (Int, List[String]) =
        try {
            val lines = Source.fromFile(fileName).getLines().toList
            val output = lines
                .dropWhile( _ != "# Output:").tail
                .takeWhile(s => s != "# Program:" && s != "# Exit:")
                .map(_.replace("#", "").trim)
                .filterNot(_.isEmpty)

            
            val exitCode: Int = lines.dropWhile( _ != "# Exit:") match
                /* 'Exit:' comment isn't always present */
                case Nil => 0
                case _::tail => 
                    tail.takeWhile(_ != "# Program:")
                    .map(_.replace("#", "").trim)
                    .filterNot(_.isEmpty)
                    .map(_.toInt)(0)
       
            return (exitCode, output) 
        } catch {
            case e: FileNotFoundException => {
                throw FileNotFoundException(s"File Not Found: $fileName")
            } 
        }

    // def temp(fileName: String) = {
    //     val progName = fileName
    //         .split("/")
    //         .last
    //         .replace(".wacc","")
        
    //     println(getExpectedOutput(fileName))
    //     println(runAssembly(progName))

    // }
    
    /** 
     * Helper function to collect all the filepaths to valid wacc programs
     * TODO: change back to all wacc programs
     */ 
    def getFilePaths(): List[String] = {
        // val fPathStart: String = "wacc-examples/valid/"
        // searchDir(File(fPathStart))
        return List("wacc-examples/valid/expressions/andExpr.wacc")
    }
}



