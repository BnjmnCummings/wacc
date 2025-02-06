package wacc

import parsley.Parsley
import parsley.Parsley.atomic
import parsley.token.{Lexer, Basic}
import parsley.token.descriptions.*
import parsley.token.errors.*

import parsley.errors.combinator.ErrorMethods

import parsley.character.{string}

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
            characterLiteralEnd = charEnd,
            stringEnds = sEnds,
            escapeSequences = EscapeDesc.plain.copy(
                escBegin = escChar,
                literals = escapedLiterals,
                mapping = escapedMapping
            ),
            graphicCharacter = Basic(c => !(escapedLiterals.contains(c)) && asciiRange(c)),
        )
    )

    private val errConfig = new ErrorConfig {
        // name errors
        override def filterNameIllFormedIdentifier: FilterConfig[String] = BadIdentConfig()

        // numeric errors
        override def labelIntegerSignedNumber: LabelWithExplainConfig = Label("number")
        override def labelIntegerNumberEnd: LabelConfig = Label("end of number")

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
                ";" -> Label("semicolon")
            )
        }

        // text errors
        override def labelCharAscii: LabelWithExplainConfig = Label("character")
        override def labelCharAsciiEnd: LabelConfig = Label("closing single quote")
        override def labelStringAscii(multi: Boolean, raw: Boolean): LabelWithExplainConfig = Label("string")
        override def labelStringAsciiEnd(multi: Boolean, raw: Boolean): LabelConfig = Label("closing double quote")
        override def labelEscapeEnd: LabelWithExplainConfig = 
            LabelAndReason(
                "valid escape sequences are \\0, \\n, \\t, \\r, \\f, \\b, \\\', \\\" and \\\\",
                "escape sequence"
                )
        // override def verifiedCharBadCharsUsedInLiteral: VerifiedBadChars = ???
        // override def verifiedStringBadCharsUsedInLiteral: VerifiedBadChars = ???

    }

    class BadIdentConfig extends Because[String] {
        override def reason(x: String): String = s"$x is an invalid identifier (identifiers must start with a letter or an underscore)"
    } 

    private val lexer = Lexer(desc, errConfig)

    val _int: Parsley[BigInt] = lexer.lexeme.integer.decimal32
    val _ident: Parsley[String] = lexer.lexeme.names.identifier
    val _char: Parsley[Char] = lexer.lexeme.character.ascii
    val _string: Parsley[String] = lexer.lexeme.string.ascii
    val _bool: Parsley[Boolean] = lexer.lexeme(atomic((string("true") as true) | (string("false") as false)))
    val implicits = lexer.lexeme.symbol.implicits
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)

    // special tokens
    val BeginProg = lexer.lexeme.symbol("begin").explain("programs must start with 'begin'")
    val EndProg = lexer.lexeme.symbol("end").explain("programs must end with 'end'")
    val ThenIf = lexer.lexeme.symbol("then").explain("if statements must have a 'then' before the body")
    val FiIf = lexer.lexeme.symbol("fi").explain("if statements must end with 'fi'")
    val WhileDo = lexer.lexeme.symbol("do").explain("while loops must have a 'do' before the body")
    val WhileDone = lexer.lexeme.symbol("done").explain("while loops must end with 'done'")
    
    def idStart(c: Char): Boolean = c.isLetter || c == '_'
    def idRest(c: Char): Boolean = c.isLetterOrDigit || c == '_'
}