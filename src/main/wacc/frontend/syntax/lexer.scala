package wacc

import wacc.error.*

import parsley.Parsley
import parsley.Parsley.atomic
import parsley.character.{string}
import parsley.token.{Lexer, Basic}
import parsley.token.descriptions.*
import parsley.token.errors.*
import parsley.errors.combinator.ErrorMethods
import parsley.errors.tokenextractors.LexToken

inline def COMMENT_START = "#"
inline def ESCAPED_LITERALS = Set('\"','\'','\\')
inline def CHAR_END = '\''
inline def STRING_ENDS = Set(("\"", "\""))
inline def ESCAPE_BEGIN = '\\'
inline def HARD_KEYWORD_SET = Set("null", "if", "then", "else", "fi", "while", 
                "do", "done", "newpair", "fst", "snd", "true", "false",
                "read", "free", "return", "exit", "print", "call",
                "println", "int", "bool", "char", "string", "pair", 
                "begin", "end", "skip", "len", "ord", "chr")

object lexer {
    /* Configuration for the lexer, specifying the lexical rules of the grammar being parsed. */
    private val desc = LexicalDesc.plain.copy(
        nameDesc = NameDesc.plain.copy(
            identifierStart = Basic(idStart),
            identifierLetter = Basic(idRest),
        ),
        spaceDesc = SpaceDesc.plain.copy(
            lineCommentAllowsEOF = true,
            lineCommentStart = COMMENT_START,
        ),
        symbolDesc = SymbolDesc.plain.copy(
            hardKeywords = HARD_KEYWORD_SET,
            caseSensitive = true,
        ),
        textDesc = TextDesc.plain.copy(
            characterLiteralEnd = CHAR_END,
            stringEnds = STRING_ENDS,
            escapeSequences = EscapeDesc.plain.copy(
                escBegin = ESCAPE_BEGIN,
                literals = ESCAPED_LITERALS,
                mapping = Map(
                    "0" -> 0x00, 
                    "b" -> 0x08,
                    "t" -> 0x09,
                    "n" -> 0x0a,
                    "f" -> 0x0c,
                    "r" -> 0x0d,
                )
            ),
            graphicCharacter = Basic(c => !(ESCAPED_LITERALS.contains(c)) && asciiRange(c)),
        )   
    )

    /* Error label definitions for lexer tokens*/
    private val errConfig = new ErrorConfig {
        override def labelIntegerSignedNumber: LabelWithExplainConfig = Label("number")
        override def labelIntegerNumberEnd: LabelConfig = Label("end of number")
        override def labelCharAscii: LabelWithExplainConfig = Label("character")
        override def labelCharAsciiEnd: LabelConfig = Label("end of character literal")
        override def labelStringAscii(multi: Boolean, raw: Boolean): LabelWithExplainConfig = Label("string")
        override def labelStringAsciiEnd(multi: Boolean, raw: Boolean): LabelConfig = Label("end of string literal")

        override def labelEscapeEnd: LabelWithExplainConfig = LabelAndReason(
            "valid escape sequences are \\0, \\n, \\t, \\r, \\f, \\b, \\\', \\\" and \\\\",
            "end of escape sequence"
        )

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
    }

    /* local lexer object */
    private val lexer = Lexer(desc, errConfig)

    /* base-type lexer tokens */
    val _int: Parsley[BigInt] = lexer.lexeme.integer.decimal32
    val _ident: Parsley[String] = lexer.lexeme.names.identifier
    val _char: Parsley[Char] = lexer.lexeme.character.ascii
    val _string: Parsley[String] = lexer.lexeme.string.ascii
    val _bool: Parsley[Boolean] = lexer.lexeme(atomic((string("true") as true) | (string("false") as false)))

    /* special tokens */
    val BeginProg = lexer.lexeme.symbol("begin").explain("programs must start with 'begin'")
    val EndProg = lexer.lexeme.symbol("end").explain("programs must end with 'end'")
    val ThenIf = lexer.lexeme.symbol("then").explain("if statements must have a 'then' before the body")
    val FiIf = lexer.lexeme.symbol("fi").explain("if statements must end with 'fi'")
    val WhileDo = lexer.lexeme.symbol("do").explain("while loops must have a 'do' before the body")
    val WhileDone = lexer.lexeme.symbol("done").explain("while loops must end with 'done'")
    val implicits = lexer.lexeme.symbol.implicits

    /* Custom error builder for Lexical Errors */
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

    /**
      * Wrapper around the [[parsley.token.Lexer]] fully function.
      * This combinator ensures a parser fully parses all available input, and consumes whitespace
      * at the start.
      * @param p a [[Parsley]] object to be parsed
      */
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)

    /**
      * Helper functions to check validity of identities.
      * An identity in WACC can start with an '_' character, otherwise it must only contain alphanumerics.
      * @param c the candidate character.
      * @return True if the character is valid.
      */
    private def idStart(c: Char): Boolean = c.isLetter || c == '_'
    private def idRest(c: Char): Boolean = c.isLetterOrDigit || c == '_'

    /**
      * Helper function to check the validity of strings.
      * String literals should only support ASCII characters
      * @param c he candidate character.
      * @return True if the character is valid.
      */
    private def asciiRange(c: Char): Boolean = c >= ' ' && c <= '~'
}
