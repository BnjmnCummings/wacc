package wacc

import sys.process._
import java.io.FileNotFoundException
import scala.io.Source
import wacc.utilities.searchDir
import java.io.File
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable.ListBuffer

class backend_integration_test extends AnyFlatSpec {
    
    "backend" should "successfully produce the desired output and exit code" in {
        runAssembly("andExpr")
        val failures: ListBuffer[String] = ListBuffer.empty[String]
        val successes: ListBuffer[String] = ListBuffer.empty[String]

        getFilePaths().foreach { 
            fileName =>
                //TODO: turn into an assembly file and put in src/test/wacc/backend/integration/assembly
                val progName = fileName
                    .split("/")
                    .last
                    .replace(".wacc","")
                
                if(runAssembly(progName) == getExpectedOutput(fileName)) {
                    successes += fileName
                } else {
                    failures += fileName
                }
        }

        val failList: List[String] = failures.toList
        val successList: List[String] = successes.toList

        /* report successful test cases */
        info("correctly succeeding tests:\n")
        successList.map(s => s.split("valid/").last).foreach(info(_))

        /* report failing test cases */
        if (failList.length != 0) {
            fail(
                "some of the paths failed (they are valid and should succeed):\n\n" 
                + failList.map(s => s.split("valid/").last).mkString("\n")
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



