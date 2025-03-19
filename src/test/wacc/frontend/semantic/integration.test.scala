package wacc.semantic

import wacc.parser
import wacc.error.ScopeException
import wacc.testUtils.searchDir

import parsley.Success

import java.io.File
import scala.collection.mutable.ListBuffer
import org.scalatest.flatspec.AnyFlatSpec

class integration_test extends AnyFlatSpec {
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
                        val (q_t, tyInfo) = renamer.rename(t)
                        typeCheck(q_t, tyInfo) match {
                            case Left(_) => semFailures += p
                            case Right(_) => successes += p 
                        }
                    } catch {
                        case e: ScopeException => {
                            // scope checking has failed
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
                + synFailList.map(s => s.split("syntaxErr/").last).mkString("\n")
            )
        }
        if (semFailList.length != 0) {
            fail(
                "some of the paths failed to scope/type check (they are semantically valid and should succeed):\n\n" 
                + semFailList.map(s => s.split("semanticErr/").last).mkString("\n")
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
                        val (q_t, tyInfo) = renamer.rename(t)
                        typeCheck(q_t, tyInfo) match {
                            case Left(_) => semFailures += p
                            case Right(_) => successes += p 
                        }
                    } catch {
                        case e: ScopeException => {
                            // scope checking has failed
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
                + synFailList.map(s => s.split("syntaxErr/").last).mkString("\n")
            )
        }
        if (successList.length != 0) {
            fail(
                "some of the paths parsed successfully (they are semantically invalid and should fail):\n\n" 
                + successList.map(s => s.split("valid/").last).mkString("\n")
            )
        }
    }

    def getValidPaths(): List[String] = {
        val fPathStart: String = "wacc-examples/valid/"
        searchDir(File(fPathStart))
    }

    def getInvalidPaths(): List[String] = {
        val fPathStart: String = "wacc-examples/invalid/semanticErr"
        searchDir(File(fPathStart))
    }
}