package wacc

import parsley.{Success, Failure}

import os._

import scala.util.Random

def main(args: Array[String]): Unit = {
    args.headOption match {
        // change parse to parseFile if you can figure out how to use it
        case Some(_) => {
            val fname = "arrayBasic.wacc"
            val fileContents: String = os.read(os.pwd / fname)
            parser.parse(fileContents) match
                case Success(x) => {
                    println(x)
                    // check semantics here 
                    sys.exit(0)
                }
                case Failure(msg) => {
                    println(msg)
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
