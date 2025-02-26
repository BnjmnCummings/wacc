package wacc

import parsley.{Success, Failure}

import wacc.semantic.*
import wacc.q_ast.Q_Prog
import wacc.t_ast.T_Prog
import wacc.assemblyIR.A_Prog
import wacc.codeGen.*

import java.io.File
import java.io.FileNotFoundException

val EXIT_SUCCESS = 0
val EXIT_SYNTAX_ERR = 100
val EXIT_SEMANTIC_ERR = 200
val EXIT_UNEXPECTED_ERR = -1

@main
def main(fname: String): Unit = {
    val (t_tree, typeInfo) = frontend(fname)
    // for now just exit successfully since frontend has completed
    sys.exit(EXIT_SUCCESS)
    backend(t_tree, typeInfo)
}

def frontend(fname: String): (T_Prog, TypeInfo) = {
    val f = new File(fname)
    val parsedFile = try {
        parser.parseF(f)
    } catch {
        case e: FileNotFoundException => {
            println(s"could not find file $fname")
            sys.exit(EXIT_UNEXPECTED_ERR)
        }
    }
    parsedFile match
        case Success(t) => {
            try {
                val (q_t, tyInfo) = renamer.rename(t, Some(fname))
                typeCheck(q_t, tyInfo, Some(fname)) match {
                    case Left(e: List[Err]) => 
                        e.foreach {
                            er => println(er.format())
                        }
                        sys.exit(EXIT_SEMANTIC_ERR)
                    // successfully parsed - return the qualified ast and type info to main
                    case Right(t_t) => return (t_t, tyInfo)
                }
                sys.exit(0)
            } catch {
                case e: ScopeException => {
                    e.messages.headOption match 
                        case Some(er) => println(er.format())
                        case None => println("no error message")
                    sys.exit(EXIT_SEMANTIC_ERR)
                }
            }
        }
        case Failure(err) => {
            println(err.format())
            sys.exit(EXIT_SYNTAX_ERR)
        }
}

def backend(t_tree: T_Prog, typeInfo: TypeInfo): Unit = {
    val assembly: A_Prog = gen(t_tree, typeInfo)
    // TODO: String output to file here @Zakk @Ben
}