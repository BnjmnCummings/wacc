package wacc.error

import parsley.errors.ErrorBuilder

/**
  * An abstract error builder for throwing front-end compilation errors.
  * Contains functions for constructing each type of error.
  * Provides an interface for the parsley library to throw our errors.
  * @see https://j-mie6.github.io/parsley/5.0/api-guide/errors/ErrorBuilder.html
  */
abstract class MyErrorBuilder extends ErrorBuilder[Err] {
    val endOfInput: EndOfInputItem.type = EndOfInputItem
    val numLinesAfter: Int = 1
    val numLinesBefore: Int = 1

    /**
      * Builds a Syntax Error
      * @param pos the position of the error.
      * @param source the source of the error.
      * @param msg the error message.
      */
    override def build(pos: (Int, Int), source: Option[String], msg: ErrorMessage): Err = 
        Err(source, pos, msg, ErrorType.SyntaxError)

    /**
      * Builds a Vanilla Error
      * @param unexpected the error item found in the code.
      * @param expecteds the item that the compiler expected to see.
      * @param reasons list of reasons why the code can't compiler.
      * @param line the line of code where the error was found.
      */
    override def vanillaError(unexpected: Option[ErrorItem], expected: Set[ErrorItem], reasons: Set[String], line: String): ErrorMessage = 
        VanillaError(unexpected, expected, reasons, line)

    /**
      * Builds a Specialized Error
      * @param msgs the messages to be printed.
      * @param line the line of code where the error was found.
      */
    override def specializedError(msgs: Set[String], line: String): ErrorMessage = 
        SpecializedError(msgs, line)

    /**
      * Invokes the generation of the code block surrounding the error.
      * @param line the line where the error occurred.
      * @param linesBefore the lines before the error.
      * @param linesAfter the lines after the error.
      * @param lineNum the line number where the error occurred.
      * @param errorPointsAt the number of characters along the line where the error occurred.
      * @param errorWidth the width of the token causing the error.
      */
    override def lineInfo(
        line: String,
        linesBefore: Seq[String],
        linesAfter: Seq[String],
        lineNum: Int, 
        errorPointsAt: Int, 
        errorWidth: Int
    ): String = genErrorMessageCodeBlock(
        line = line, 
        linesBefore = linesBefore, 
        linesAfter = linesAfter,
        errorPointsAt = errorPointsAt,
        errorWidth = errorWidth
    )
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

    type Position = (Int, Int)
    type Source = Option[String]
    type ErrorInfoLines = ErrorMessage
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

