package wacc.ast

import wacc.parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import parsley.{Success, Failure}

import collection.mutable.ListBuffer

import java.io.File

class syntax_integration_test extends AnyFlatSpec {
    val validPaths: List[String] = getValidPaths()

    val invalidPaths: List[String] = getInvalidPaths()

    "parser" should "successfully parse valid wacc programs" in {
        val failures: ListBuffer[String] = new ListBuffer()
        val successes: ListBuffer[String] = new ListBuffer()
        validPaths.foreach {
            p => parser.parseF(File(p)) match 
                case Success(_) => successes += p
                case _ => failures += p
        }
        val failList: List[String] = failures.toList
        val successList: List[String] = successes.toList
        info("correctly succeeding tests:\n")
        successList.map(s => s.split("valid/").last).foreach(info(_))
        if (failList.length != 0) {
            fail(
                "some of the paths failed (they are valid and should succeed):\n\n" 
                + failList.map(s => s.split("valid/").last).mkString("\n")
            )
        }
    }

    it should "reject invalid wacc programs" in {
        val successes: ListBuffer[String] = new ListBuffer()
        val failures: ListBuffer[String] = new ListBuffer()
        invalidPaths.foreach {
            p => parser.parseF(File(p)) match 
                case Failure(_) => failures += p
                case _ => successes += p
        }
        val successList: List[String] = successes.toList
        val failList: List[String] = failures.toList
        info("correctly failing tests:\n")
        failList.map(s => s.split("syntaxErr/").last).foreach(info(_))
        if (successList.length != 0) {
            fail(
                "some of the paths succeeded (they are invalid and should fail):\n\n" 
                + successList.map(s => s.split("syntaxErr/").last).mkString("\n")
            )
        }
    }

    def getValidPaths(): List[String] = {
        val fPathStart: String = "wacc-examples/valid/"
        searchFiles(File(fPathStart))
    }

    def getInvalidPaths(): List[String] = {
        val fPathStart: String = "wacc-examples/invalid/syntaxErr"
        searchFiles(File(fPathStart))
    }


    // Function to recursively search for files in the given directory
    // Chatgpt wrote this one
    def searchFiles(dir: File): List[String] = {
        if (dir.exists && dir.isDirectory) {
            // List to collect all file paths
            val filePaths = dir.listFiles.filter(_.isFile).map(_.getAbsolutePath).toList

            // Recursively search in subdirectories
            val subDirFiles = dir.listFiles.filter(_.isDirectory).flatMap(searchFiles).toList

            // Combine files from current directory and subdirectories
            filePaths ++ subDirFiles
        } else {
            List()  // Return empty list if the path is not a valid directory
        }
    }
}