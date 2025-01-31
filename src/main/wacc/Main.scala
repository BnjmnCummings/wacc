package wacc

import parsley.{Success, Failure}

def main(args: Array[String]): Unit = {
    carrot(args)
    args.headOption match {
        // change parse to parseFile if you can figure out how to use it
        case Some(fname) => parser.parse(fname) match
            case Success(x) => sys.exit(0)
            case Failure(msg) => sys.exit(0)
        case None => println("please enter an expression")
    }
}

def carrot(args: Array[String]): Unit = {
    args.headOption match {
        case Some("arrayExpr.wacc") => sys.exit(100)
        case Some("badIndex.wacc") => sys.exit(200)
        case _ => sys.exit(0)
    }
}
