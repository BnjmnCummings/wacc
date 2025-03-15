package wacc

import parsley.Parsley
import parsley.Parsley.atomic
import parsley.character.{string}
import parsley.token.{Lexer, Basic}
import parsley.token.descriptions.*
import parsley.token.errors.*
import parsley.errors.combinator.ErrorMethods
import parsley.errors.tokenextractors.LexToken

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
        override def labelCharAsciiEnd: LabelConfig = Label("end of character literal")
        override def labelStringAscii(multi: Boolean, raw: Boolean): LabelWithExplainConfig = Label("string")
        override def labelStringAsciiEnd(multi: Boolean, raw: Boolean): LabelConfig = Label("end of string literal")
        override def labelEscapeEnd: LabelWithExplainConfig = 
            LabelAndReason(
                "valid escape sequences are \\0, \\n, \\t, \\r, \\f, \\b, \\\', \\\" and \\\\",
                "end of escape sequence"
                )

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
    val ThenIf = lexer.lexeme.symbol("then").explain("if statements must have a 'then' before the body")
    val FiIf = lexer.lexeme.symbol("fi").explain("if statements must end with 'fi'")
    val WhileDo = lexer.lexeme.symbol("do").explain("while loops must have a 'do' before the body")
    val WhileDone = lexer.lexeme.symbol("done").explain("while loops must end with 'done'")
    
    def idStart(c: Char): Boolean = c.isLetter || c == '_'
    def idRest(c: Char): Boolean = c.isLetterOrDigit || c == '_'

    val LexErrorBuilder = new MyErrorBuilder with LexToken {
        def tokens = Seq(
            lexer.nonlexeme.integer.decimal32.map(n => s"integer $n"),
            lexer.nonlexeme.names.identifier.map(v => s"identifier $v"),
            lexer.nonlexeme.character.ascii.map(c => s"character $c"),
            lexer.nonlexeme.string.ascii.map(s => s"string $s"),
            atomic((string("true") as true) | (string("false") as false)).map(b => s"$b")
        ) ++ desc.symbolDesc.hardKeywords.map { 
            k => lexer.nonlexeme.symbol(k).as(s"keyword $k")
        }
    }
}