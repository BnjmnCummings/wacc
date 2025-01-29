package wacc

import parsley.Parsley
import parsley.token.{Lexer, Basic, Unicode}
import parsley.token.descriptions.*

import scala.collection.immutable.NumericRange

val FIRST_GRAPHIC_ASCII = '!'
val LAST_GRAPHIC_ASCII = '~'

object lexer {
    private val desc = LexicalDesc.plain.copy(
        nameDesc = NameDesc.plain.copy(
            identifierStart = Basic(_.isLetter),
            identifierLetter = Basic(_.isLetterOrDigit),
        ),
        spaceDesc = SpaceDesc.plain.copy(
            lineCommentAllowsEOF = true,
            lineCommentStart = "#",
        ),
        symbolDesc = SymbolDesc.plain.copy(
            hardKeywords = Set("null", "if", "then", "else", "fi", "while", 
                "do", "done", "for", "newpair", "fst", "snd", 
                "read", "free", "return", "exit", "print", "call",
                "println", "int", "bool", "char", "string", "pair", 
                "begin", "end", "skip", "len", "ord", "chr"),
            caseSensitive = true,
        ),
        textDesc = TextDesc.plain.copy(
            characterLiteralEnd = '\'',
            stringEnds = Set(("\"", "\"")),
            escapeSequences = EscapeDesc.plain.copy(
                escBegin = '\\',
                literals = Set('\"','\'','\\'),
                mapping = Map(
                    "0" -> 0x00, 
                    "b" -> 0x08,
                    "t" -> 0x09,
                    "n" -> 0x0a,
                    "f" -> 0x0c,
                    "r" -> 0x0d,
                ),
            ),
            graphicCharacter = 
                // Not sure if \ " and ' should be included or excluded here
                Basic(NumericRange.inclusive[Char](FIRST_GRAPHIC_ASCII,LAST_GRAPHIC_ASCII,1)),
        )
    )
    private val lexer = Lexer(desc)

    val _int: Parsley[BigInt] = lexer.lexeme.integer.decimal32
    val _ident: Parsley[String] = lexer.lexeme.names.identifier
    val implicits = lexer.lexeme.symbol.implicits
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
