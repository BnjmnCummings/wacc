package wacc

import parsley.{Success, Failure}

import scala.util.Random

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
    val randomNumber = Random.nextInt(3)
    randomNumber match {
        case 0 => sys.exit(0)    
        case 1 => sys.exit(100)  
        case 2 => sys.exit(200)  
    }
}
