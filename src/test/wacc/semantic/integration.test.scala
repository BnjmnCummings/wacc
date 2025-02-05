package wacc.ast

import wacc.parser
import wacc.renamer
import wacc.typeChecker
import wacc.ScopeException

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import parsley.{Success, Failure}

import collection.mutable.ListBuffer

import java.io.File

class semantic_integration_test extends AnyFlatSpec {
    val validPaths: List[String] = getValidPaths()

    val invalidPaths: List[String] = getInvalidPaths()

    "parser" should "successfully parse and scope/type check valid wacc programs" in {
        val synFailures: ListBuffer[String] = new ListBuffer()
        val semFailures: ListBuffer[String] = new ListBuffer()
        val successes: ListBuffer[String] = new ListBuffer()
        validPaths.foreach {
            p => parser.parseF(File(p)) match 
                case Success(t) => {
                    try {
                        val q_t = renamer.rename(t)
                        typeChecker.check(q_t)
                        successes += p
                    } catch {
                        case e: ScopeException => {
                            // type/scope checking has failed
                            semFailures += p
                        }
                    }
                }
                case _ => synFailures += p
        }
        val synFailList: List[String] = synFailures.toList
        val semFailList: List[String] = semFailures.toList
        val successList: List[String] = successes.toList
        info("programs identified correctly as valid:\n")
        successList.map(s => s.split("valid/").last).foreach(info(_))
        if (synFailList.length != 0) {
            fail(
                "some of the paths failed to parse (they are syntactically valid and should succeed):\n\n" 
                + synFailList.foldRight("")((s1, s2) => s1 + "\n" + s2)
            )
        }
        if (semFailList.length != 0) {
            fail(
                "some of the paths failed to scope/type check (they are semantically valid and should succeed):\n\n" 
                + semFailList.foldRight("")((s1, s2) => s1 + "\n" + s2)
            )
        }
    }

    it should "reject semantically invalid wacc programs as semantically invalid" in {
        val synFailures: ListBuffer[String] = new ListBuffer()
        val semFailures: ListBuffer[String] = new ListBuffer()
        val successes: ListBuffer[String] = new ListBuffer()
        invalidPaths.foreach {
            p => parser.parseF(File(p)) match 
                case Success(t) => {
                    try {
                        val q_t = renamer.rename(t)
                        typeChecker.check(q_t)
                        successes += p
                    } catch {
                        case e: ScopeException => {
                            // type/scope checking has failed
                            semFailures += p
                        }
                    }
                }
                case _ => synFailures += p
        }
        val synFailList: List[String] = synFailures.toList
        val semFailList: List[String] = semFailures.toList
        val successList: List[String] = successes.toList
        info("programs identified correctly as semantically invalid:\n")
        semFailList.map(s => s.split("semanticErr/").last).foreach(info(_))
        if (synFailList.length != 0) {
            fail(
                "some of the paths failed to parse (they are syntactically valid and should succeed):\n\n" 
                + synFailList.foldRight("")((s1, s2) => s1 + "\n" + s2)
            )
        }
        if (successList.length != 0) {
            fail(
                "some of the paths parsed successfully (they are semantically invalid and should fail):\n\n" 
                + semFailList.foldRight("")((s1, s2) => s1 + "\n" + s2)
            )
        }
    }

    def getValidPaths(): List[String] = {
        val fPathStart: String = "wacc-examples/valid/"
        searchFiles(File(fPathStart))
    }

    def getInvalidPaths(): List[String] = {
        val fPathStart: String = "wacc-examples/invalid/semanticErr"
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