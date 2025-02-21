package wacc

import sys.process._
import java.io.FileNotFoundException
import scala.io.Source
//import wacc.utilities.searchDir
import java.io.File
//import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable.ListBuffer

// class backend_integration_test extends AnyFlatSpec {
    
//     "backend" should "successfully produce the desired output and exit code" in {

//         runAssembly("/homes/bc2423/Wacc/WACC_16/src/test/wacc/backend/andExpr.s")
//         // val failures: ListBuffer[String] = ListBuffer.empty[String]
//         // val successes: ListBuffer[String] = ListBuffer.empty[String]

//         // getFilePaths().foreach { 
//         //     filename =>
//         //         //turn into an assembly file
//         //         if(runAssembly(filename) == getExpectedOutput(filename)) {
//         //             successes += filename
//         //         } else {
//         //             failures += filename
//         //         }
//         // }

//         // val failList: List[String] = failures.toList
//         // val successList: List[String] = successes.toList

//         // /* report successful test cases */
//         // info("correctly succeeding tests:\n")
//         // successList.map(s => s.split("valid/").last).foreach(info(_))

//         // /* report failing test cases */
//         // if (failList.length != 0) {
//         //     fail(
//         //         "some of the paths failed (they are valid and should succeed):\n\n" 
//         //         + failList.map(s => s.split("valid/").last).mkString("\n")
//         //     )
//         // }
//     } 

//     // TODO: seperate the sourcefile with the generated files: perhaps into a build folder
// }

/**
  * 
  * @param filename the path to the generated 'progName.s' assembly file
  * @return a pair: (exit code, output)
  */
def runAssembly(filename: String): (Int, List[String]) = {
        /* TODO: refactor with regex*/
        val progName = "assembly/" + filename
            .split("/")
            .last
            .replace(".s","")

        val buildExitStatus = s"./buildAss $progName" .!

        if (buildExitStatus == 0) {
            val cmd = s"./$progName"
            val exitStatus = cmd .!
            val output: ListBuffer[String] = ListBuffer.empty[String]

            if(exitStatus == 0) {
                output ++= (cmd .!!).split('\n')
            }
            //clean up after ourselves and return
            s"./wipeAss $progName" .!
            return (exitStatus, output.toList)

        } else {
            throw InstantiationException(s"Build command failed with exit status: $buildExitStatus")
        }
    }

    /**
     * @param filename the path to the .wacc file
     * @return an expected pair: (exit code, output)
     */
    def getExpectedOutput(filename: String): (Int, List[String]) =
        try {
            val lines = Source.fromFile(filename).getLines().toList
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
                throw FileNotFoundException(s"File Not Found: $filename")
            } 
        }

    //@main
    def temp(filename: String) = {
        val progName = filename
            .split("/")
            .last
            .replace(".wacc","")
        val assName = s"src/test/wacc/backend/integration/assembly/$progName"
        
        println(getExpectedOutput(filename))
        println(runAssembly(assName))

        //println(s"result:${ getExpectedOutput(filename) == runAssembly(progName)}")
    }

    // def getFilePaths(): List[String] = {
    //     val fPathStart: String = "wacc-examples/valid/"
    //     searchDir(File(fPathStart))
    // }