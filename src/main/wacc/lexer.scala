package wacc

import parsley.Parsley
import parsley.Parsley.atomic
import parsley.token.{Lexer, Basic}
import parsley.token.descriptions.*
import parsley.token.errors.*

import parsley.character.{string}
// import lexer.implicits.implicitSymbol

object lexer {
    private val desc = LexicalDesc.plain.copy(
        nameDesc = NameDesc.plain.copy(
            identifierStart = Basic(idStart),
            identifierLetter = Basic(idRest),
        ),
        spaceDesc = SpaceDesc.plain.copy(
            lineCommentAllowsEOF = true,
            lineCommentStart = commentStart,
        ),
        symbolDesc = SymbolDesc.plain.copy(
            hardKeywords = hardKwdsSet,
            caseSensitive = true,
        ),
        textDesc = TextDesc.plain.copy(
            // characterLiteralEnd = charEnd,
            // stringEnds = sEnds,
            escapeSequences = EscapeDesc.plain.copy(
                escBegin = escChar,
                literals = escapedLiterals,
                mapping = escapedMapping
            ),
            graphicCharacter = Basic(c => !(escapedLiterals.contains(c)) && c >= ' '.toInt),
        )
    )

    private val errConfig = new ErrorConfig {
        // name errors
        // override def filterNameIllFormedIdentifier: FilterConfig[String] = ???
        // override def labelNameIdentifier: String = ???
        // override def unexpectedNameIllegalIdentifier(v: String): String = ???

        // // numeric errors
        // override def filterIntegerOutOfBounds(min: BigInt, max: BigInt, nativeRadix: Int): FilterConfig[BigInt] = ???
        // override def labelIntegerSignedNumber: LabelWithExplainConfig = ???
        // override def labelIntegerUnsignedNumber: LabelWithExplainConfig = ???

        // // space errors
        // override def labelSpaceEndOfLineComment: LabelWithExplainConfig = ???

        // symbol errors
        override def labelSymbol: Map[String, LabelWithExplainConfig] = {
            Map(
                "+" -> Label("arithmetic operator"), 
                "*" -> Label("arithmetic operator"),
                "-" -> Label("arithmetic operator"),
                "/" -> Label("arithmetic operator"),
                "%" -> Label("arithmetic operator"),
                ">" -> Label("comparison operator"),
                ">=" -> Label("comparison operator"),
                "<" -> Label("comparison operator"),
                "<=" -> Label("comparison operator"),
                "==" -> Label("comparison operator"), 
                "!=" -> Label("comparison operator"), 
                "&&" -> Label("boolean operator"), 
                "||" -> Label("boolean operator"),
                "!" -> Label("prefix operator"),
                "len" -> Label("prefix operator"),
                "ord" -> Label("prefix operator"),
                "chr" -> Label("prefix operator"),
                "(" -> Label("bracketed expression"),
                ")" -> LabelAndReason("unclosed brackets", "closing bracket"),
            )
        }

        // override def labelSymbolEndOfKeyword(symbol: String): String = ???

        // // text errors
        // override def filterCharNonAscii: VanillaFilterConfig[Int] = ???
        // override def filterStringNonAscii: SpecializedFilterConfig[StringBuilder] = ???
        // override def labelCharAscii: LabelWithExplainConfig = ???
        // override def labelCharAsciiEnd: LabelConfig = ???
        // override def labelEscapeSequence: LabelWithExplainConfig = ???
        // override def labelGraphicCharacter: LabelWithExplainConfig = ???
        // override def labelStringAscii(multi: Boolean, raw: Boolean): LabelWithExplainConfig = ???
        // override def labelStringAsciiEnd(multi: Boolean, raw: Boolean): LabelConfig = ???
        // override def verifiedCharBadCharsUsedInLiteral: VerifiedBadChars = ???
        // override def verifiedStringBadCharsUsedInLiteral: VerifiedBadChars = ???

    }

    private val lexer = Lexer(desc, errConfig)

    val _int: Parsley[BigInt] = lexer.lexeme.integer.decimal32
    val _ident: Parsley[String] = lexer.lexeme.names.identifier
    val _char: Parsley[Char] = lexer.lexeme.character.ascii
    val _string: Parsley[String] = lexer.lexeme.string.ascii
    val _bool: Parsley[Boolean] = lexer.lexeme(atomic((string("true") as true) | (string("false") as false)))
    val implicits = lexer.lexeme.symbol.implicits
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)

    def idStart(c: Char): Boolean = c.isLetter || c == '_'
    def idRest(c: Char): Boolean = c.isLetterOrDigit || c == '_'
}