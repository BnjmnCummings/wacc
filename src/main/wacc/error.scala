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
        // need to deal with error lines here
        sb.addAll(???)
        sb.toString()
    }
}

sealed trait ErrorLines

case class VanillaError(
    unexpected: Option[ErrorItem],
    expecteds: Set[ErrorItem],
    reasons: Set[String],
) extends ErrorLines

case class SpecializedError(
    msgs: Set[String]
) extends ErrorLines

sealed trait ErrorItem
case class RawItem(item: String) extends ErrorItem
case class NamedItem(item: String) extends ErrorItem
case object EndOfInputItem extends ErrorItem

enum ErrorType {
    case SyntaxError
    case SemanticError
}
