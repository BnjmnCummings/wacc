package wacc.semantic

import wacc.parser

import parsley.{Failure, Success}

import wacc.Err

import wacc.renamer
import wacc.ScopeException

import wacc.t_ast.T_Prog

def parseAndTypeCheckStr(inpString: String): Either[List[Err], T_Prog] = {
    parser.parse(inpString) match
        case Failure(msg) => throw new Exception(s"didn't parse syntactically for some reason, here is the message:\n $msg")
        case Success(x) => {
            try {
                val (q_t, tyInfo) = renamer.rename(x)
                wacc.semantic.typeCheck(q_t, tyInfo) 
            }
            catch {
                case e: ScopeException => throw e
            }
        }
}
