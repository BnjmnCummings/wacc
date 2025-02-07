package wacc

import parsley.{Success, Failure}

import scala.util.Random

import wacc.semantic.*

import java.io.File

def main(args: Array[String]): Unit = {
    args.headOption match {
        // change parse to parseFile if you can figure out how to use it
        case Some(fname) => {
            val f = new File(fname)
            parser.parseF(f) match
                case Success(t) => {
                    try {
                        val (q_t, tyInfo) = renamer.rename(t, Some(fname))
                        typeCheck(q_t, tyInfo, Some(fname)) match {
                            case Some(e: List[Err]) => 
                                e.foreach {
                                    er => println(er.format())
                                }
                                sys.exit(200)
                            case None => sys.exit(0) 
                        }
                        sys.exit(0)
                    } catch {
                        case e: ScopeException => {
                            // some kind of unified error messaging here
                            e.messages.headOption match 
                                case Some(er) => println(er.format())
                                case None => println("no error message for some reason")
                            sys.exit(200)
                        }
                    }
                    
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

