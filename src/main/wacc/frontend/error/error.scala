package wacc.error

private inline def CODE_INDENT: String = " ".repeat(2)

/**
  * A trait to represent Error Items.
  * These usually represent unexpected tokens caught by the compiler.
  */
sealed trait ErrorItem
case class RawItem(item: String) extends ErrorItem
case class NamedItem(item: String) extends ErrorItem
case object EndOfInputItem extends ErrorItem

/**
  * An enum for representing the two classes of errors that can occur in the compilers front-end
  */
enum ErrorType {
    case SyntaxError
    case SemanticError
}

/**
  * Class to represent a compilation error.
  * @param fname the filename of the program being compiled.
  * @param pos the position (line, char) where the error occurred.
  * @param msg the error message to be printed.
  * @param errType the type of error that occured.
  */
class Err(fname: Option[String], pos: (Int, Int), msg: ErrorMessage,  errType: ErrorType) {
    /**
      * Formats the complete error message into a string.
      * @return the error message containing: the position, error type, filename and msg.
      */
    def format(): String = 
        val sb = StringBuilder()
        val fileInfo: String = fname match 
            case Some(f) => s"$f "
            case None => "unknown file "

        val eTypeStr: String = errType match 
            case ErrorType.SemanticError => "Semantic error in "
            case ErrorType.SyntaxError => "Syntax error in "

        sb ++= eTypeStr
        sb ++= fileInfo
        sb ++= s"at (line: ${pos._1}, column: ${pos._2}):\n"
        sb ++= msg.toString()

        sb.toString()
}

/**
 * A trait to represent all types of error messages.
 */
sealed trait ErrorMessage

/**
  * A class to represent 'vanilla errors' in the form "unexpected: _ . expected: _ " etc. 
  * @param unexpected the error item found in the code.
  * @param expecteds the item that the compiler expected to see.
  * @param reasons list of reasons why the code can't compiler.
  * @param line the line of code where the error was found.
  */
case class VanillaError(unexpected: Option[ErrorItem], expecteds: Set[ErrorItem], reasons: Set[String], line: String) extends ErrorMessage {
    
    /**
      * A toString implementation that formats all of our information into an error message
      */
    override def toString(): String = 
        val sb = StringBuilder()
        val unexpectedStr: String = unexpected match
            case Some(item) =>
                item match
                    case RawItem(s) => s"${CODE_INDENT}unexpected \"$s\"\n"
                    case NamedItem(s) => s"${CODE_INDENT}unexpected $s\n" 
                    case EndOfInputItem => s"${CODE_INDENT}unexpected end of input\n" 
            case None => ""

        val expectedStrs: Set[String] = expecteds.map { item => 
            item match
                case RawItem(s) => s"\"$s\""
                case NamedItem(s) => s
                case EndOfInputItem => "end of input"
        }

        sb ++= unexpectedStr
        sb ++= s"${CODE_INDENT}expected "
        sb ++= formatWordSet(expectedStrs)
        sb += '\n'
        sb ++= CODE_INDENT
        sb ++= reasons.mkString(s"\n$CODE_INDENT")
        if (!reasons.isEmpty) sb += '\n'  
        sb += '\n'
        sb ++= line

        sb.toString()

    /**
      * A helper function for formatting a list of expected keywords.
      * @param words the strings/tokens that the compiler would have excepted
      * @return a formated string of words.
      */
    private def formatWordSet(words: Set[String]): String = 
        val wordList: List[String] = words.toList.sorted 
        wordList match 
            case Nil => ""
            case List(w: String) => w
            case _ => words.init.mkString(", ") + " or " + words.last
}

/**
  * A class to represent more specialised errors.
  * @param msgs
  * @param line
  */
case class SpecializedError(msgs: Set[String], line: String) extends ErrorMessage {
    /**
      * A toString implementation that formats all of our information into an error message
      */
    override def toString(): String = 
        val sb = StringBuilder()

        sb ++= CODE_INDENT
        sb ++= msgs.mkString(s"\n$CODE_INDENT")
        if (!msgs.isEmpty) sb += '\n'
        sb += '\n'
        sb ++= line

        sb.toString()
}

/**
  * A function that generates the code block surrounding the error.
  * @param line the line where the error occurred.
  * @param linesBefore the lines before the error.
  * @param linesAfter the lines after the error.
  * @param errorPointsAt the number of characters along the line where the error occurred.
  * @param errorWidth the width of the token causing the error.
  */
def genErrorMessageCodeBlock(line: String, linesBefore: Seq[String], linesAfter: Seq[String], errorPointsAt: Int, errorWidth: Int): String = 
    val sb = StringBuilder()
    sb ++= s"${CODE_INDENT}> "
    linesBefore.map(_ + s"\n${CODE_INDENT}> ").foreach{sb ++= _}
    sb ++= line
    sb ++= s"\n${CODE_INDENT}> "
    sb ++= " ".repeat(errorPointsAt) + "^".repeat(errorWidth) + s"\n${CODE_INDENT}> "
    linesAfter.map(_ + s"\n${CODE_INDENT}> ").foreach{sb ++= _}
    sb.dropRight(3 + CODE_INDENT.length()).toString() // remove extra newline
