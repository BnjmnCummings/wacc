package wacc

val codeIndent: String = " ".repeat(2)

class Err(
    fname: Option[String],
    pos: (Int, Int),
    lines: ErrorLines,
    errType: ErrorType
) {
    def format(): String = {
        val sb = StringBuilder()
        val eTypeStr: String = errType match {
            case ErrorType.SemanticError => "Semantic error in "
            case ErrorType.SyntaxError => "Syntax error in "
        }
        sb ++= eTypeStr
        val fileInfo: String = fname match {
            case Some(f) => s"$f "
            case None => "unknown file "
        }
        sb ++= fileInfo
        sb ++= s"at (line: ${pos._1}, column: ${pos._2}):\n"
        sb ++= lines.toString()
        sb.toString()
    }
}

sealed trait ErrorLines

case class VanillaError(
    unexpected: Option[ErrorItem],
    expecteds: Set[ErrorItem],
    reasons: Set[String],
    line: String
) extends ErrorLines {
    override def toString(): String = {
        val sb = StringBuilder()
        val unexpectedStr: String = unexpected match
            case Some(item) =>
                item match
                    case RawItem(s) => s"${codeIndent}unexpected \"$s\"\n"
                    case NamedItem(s) => s"${codeIndent}unexpected $s\n" 
                    case EndOfInputItem => s"${codeIndent}unexpected end of input\n" 
            case None => ""
        sb ++= unexpectedStr
        val expectedStrs: Set[String] = expecteds.map {
            item => item match {
                case RawItem(s) => s"\"$s\""
                case NamedItem(s) => s
                case EndOfInputItem => "end of input"
            }
        }
        sb ++= s"${codeIndent}expected "
        sb ++= expectedStrs.mkString(", ")
        sb += '\n'
        sb ++= reasons.mkString("\n")
        sb += '\n'
        sb ++= line
        sb.toString()
    }
}

case class SpecializedError(
    msgs: Set[String],
    line: String
) extends ErrorLines {
    override def toString(): String = {
        val sb = StringBuilder()
        sb.addAll(msgs.mkString("\n"))
        sb.addOne('\n')
        sb.addAll(line)
        sb.toString()
    }
}

sealed trait ErrorItem
case class RawItem(item: String) extends ErrorItem
case class NamedItem(item: String) extends ErrorItem
case object EndOfInputItem extends ErrorItem

enum ErrorType {
    case SyntaxError
    case SemanticError
}
