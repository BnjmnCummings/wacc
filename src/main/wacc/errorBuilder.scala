package wacc

import parsley.errors.ErrorBuilder
// import parsley.errors.Token

abstract class MyErrorBuilder extends ErrorBuilder[Err] {
    override def build(
        pos: (Int, Int), 
        source: Option[String],
        // build is only used for syntax errors
        lines: ErrorLines): Err = Err(source, pos, lines, ErrorType.SyntaxError)

    override def vanillaError(
        unexpected: Option[ErrorItem],
        expected: Set[ErrorItem],
        reasons: Set[String],
        line: String
    ): ErrorInfoLines = VanillaError(unexpected, expected, reasons, line)

    override def specializedError(
        msgs: Set[String],
        line: String
      ): ErrorLines = SpecializedError(msgs, line)

    override def pos(line: Int, col: Int): (Int, Int) = (line, col)
    override def source(sourceName: Option[String]): Option[String] = sourceName
    override def combineExpectedItems(alts: Set[ErrorItem]): Set[ErrorItem] = alts
    override def combineMessages(alts: Seq[String]): Set[String] = alts.toSet
    override def unexpected(item: Option[ErrorItem]): Option[ErrorItem] = item
    override def expected(alts: Set[ErrorItem]): Set[ErrorItem] = alts
    override def message(msg: String): String = msg
    override def reason(msg: String): String = msg
    override def raw(item: String): RawItem = RawItem(item)
    override def named(item: String): NamedItem = NamedItem(item)
    val endOfInput: EndOfInputItem.type = EndOfInputItem

    val numLinesAfter: Int = 0
    val numLinesBefore: Int = 0
    override def lineInfo(
        line: String,
        linesBefore: Seq[String],
        linesAfter: Seq[String],
        lineNum: Int, errorPointsAt: Int, errorWidth: Int
        ): String = {
            val sb = StringBuilder()
            linesBefore.foreach{sb.addAll(_)}
            sb.addOne('\n')
            sb.addAll(line)
            sb.addOne('\n')
            sb.addAll(" ".repeat(errorPointsAt) + "^".repeat(errorWidth) + "\n")
            linesAfter.foreach{sb.addAll(_)}
            sb.addOne('\n')
            sb.toString()
        }

    // The implementation of this is usually provided by a mixed-in
    // token extractor, discussed in `tokenextractors`
    /*
    def unexpectedToken(
        cs: Iterable[Char],
        amountOfInputParserWanted: Int,
        lexicalError: Boolean
    ): Token = ???
    */

    type Position = (Int, Int)
    type Source = Option[String]
    type ErrorInfoLines = ErrorLines
    type Item = ErrorItem
    type Raw = RawItem
    type Named = NamedItem
    type EndOfInput = EndOfInputItem.type
    type Message = String
    type Messages = Set[String]
    type ExpectedItems = Set[ErrorItem]
    type ExpectedLine = Set[ErrorItem]
    type UnexpectedLine = Option[ErrorItem]
    type LineInfo = String
}

