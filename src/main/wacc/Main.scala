package wacc

import parsley.{Success, Failure}

import scala.util.Random

import java.io.File

def main(args: Array[String]): Unit = {
    args.headOption match {
        // change parse to parseFile if you can figure out how to use it
        case Some(fname) => {
            val f = new File(fname)
            parser.parseF(f) match
                case Success(x) => {
                    // check semantics here 
                    println(x)
                    sys.exit(0)
                }
                case Failure(err) => {
                    println(err.format())
                    sys.exit(100)
                }
        }
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

