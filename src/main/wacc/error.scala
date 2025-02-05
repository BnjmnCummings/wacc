package wacc

class Err(
    fname: Option[String],
    pos: (Int, Int),
    lines: ErrorLines,
    errType: ErrorType
) {
    def format(): String = {
        val sb = StringBuilder()
        val eTypeStr: String = errType match {
            case ErrorType.SemanticError => "Semantic Error\n"
            case ErrorType.SyntaxError => "Syntax Error\n"
        }
        sb.addAll(eTypeStr)
        val fileInfo: String = fname match {
            case Some(f) => s"In file $f\n"
            case None => "No file location found\n"
        }
        sb.addAll(fileInfo)
        sb.addAll(s"At line: ${pos._1} column: ${pos._2}\n")
        sb.addAll(lines.toString())
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
                    case RawItem(s) => s"unexpected \"$s\"\n"
                    case NamedItem(s) => s"unexpected $s\n" 
                    case EndOfInputItem => "unexpected end of input\n" 
            case None => ""
        sb.addAll(unexpectedStr)
        val expectedStrs: Set[String] = expecteds.map {
            item => item match {
                case RawItem(s) => s"\"s\""
                case NamedItem(s) => s
                case EndOfInputItem => "end of input"
            }
        }
        sb.addAll("expected ")
        sb.addAll(expectedStrs.mkString(", "))
        sb.addOne('\n')
        sb.addAll(reasons.mkString("\n"))
        sb.addOne('\n')
        sb.addAll(line)
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
