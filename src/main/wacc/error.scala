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
        sb.addAll(expectedStrs.foldLeft("")((s1, s2) => s"$s1, $s2"))
        sb.addOne('\n')
        sb.addAll(reasons.foldLeft("")((s1, s2) => s"$s1\n $s2"))
        sb.addOne('\n')
        sb.toString()
    }
}

case class SpecializedError(
    msgs: Set[String]
) extends ErrorLines {
    override def toString(): String = msgs.foldLeft("")((s1, s2) => s"$s1\n $s2")
}

sealed trait ErrorItem
case class RawItem(item: String) extends ErrorItem
case class NamedItem(item: String) extends ErrorItem
case object EndOfInputItem extends ErrorItem

enum ErrorType {
    case SyntaxError
    case SemanticError
}
