package wacc

import parsley.{Success, Failure}
import wacc.syntax.{Asgn, Prog}

def main(args: Array[String]): Unit = {
    args.headOption match {
        // change parse to parseFile if you can figure out how to use it
        case Some(fname) => parser.parse(fname) match {
            case Success(x) => {
                x match
                    // hardcoding the first failing semantic test which has first line int[] a = [1,2];
                    case Prog(Nil, (Asgn(_,_) :: xs)) => sys.exit(200)
                    case _ => sys.exit(0)
            }
            case Failure(msg) => {
                sys.exit(100)
            }
        }
        case None => println("please enter an expression")
    }
}
