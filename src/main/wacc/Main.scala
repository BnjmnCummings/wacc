package wacc

import parsley.{Success, Failure}

import wacc.semantic.*
import wacc.t_ast.T_Prog
import wacc.assemblyIR.A_Prog
import wacc.codeGen.*
import wacc.formatting.*

import java.io.File
import java.io.FileNotFoundException

val EXIT_SUCCESS = 0
val EXIT_SYNTAX_ERR = 100
val EXIT_SEMANTIC_ERR = 200
val EXIT_UNEXPECTED_ERR = -1

/**
  * The main entry point of the compiler.
  * Parses the .wacc file at fname into x86 assembly.
  * @param fname the filename of the .wacc file.
  */
@main
def main(fname: String): Unit = {
    val (t_tree, typeInfo) = frontend(fname)
    backend(t_tree, typeInfo, fname)
}

/**
  * The frontend pipeline.
  * Handles syntactical and semantic analysis of a .wacc program
  * @param fname the filename of the .wacc file.
  * @return The typed AST of the program and type information about the functions and variables.
  */
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
        case Success(t) => 
            try {
                val (q_t, tyInfo) = renamer.rename(t, Some(fname))
                typeCheck(q_t, tyInfo, Some(fname)) match 
                    case Left(e: List[Err]) => 
                        e.foreach {
                            er => println(er.format())
                        }
                        sys.exit(EXIT_SEMANTIC_ERR)
                    /* successfully parsed - return the qualified ast and type info to main */
                    case Right(t_t) => return (t_t, tyInfo)
                
                sys.exit(EXIT_SUCCESS)
            } catch {
                case e: ScopeException => 
                    e.messages.headOption match 
                        case Some(er) => println(er.format())
                        case None => println("no error message")

                    sys.exit(EXIT_SEMANTIC_ERR)
            }

        case Failure(err) => 
            println(err.format())
            sys.exit(EXIT_SYNTAX_ERR)
}

/**
  * The frontend pipeline.
  * Handles code generation and produces a progName.s assembly file
  * @param t_tree the typed AST of the program to be run.
  * @param typeInfo type information about the functions and variables.
  * @param fname the filename of the .wacc file.
  */
def backend(t_tree: T_Prog, typeInfo: TypeInfo, fname: String): Unit = 
    val assembly: A_Prog = gen(t_tree, typeInfo)
    val progName = fname
                .split("/")
                .last
                .replace(".wacc", "")
    val printWriter = new java.io.PrintWriter(s"$progName.s")

    formatProg(assembly)(using printWriter)
    printWriter.close()
