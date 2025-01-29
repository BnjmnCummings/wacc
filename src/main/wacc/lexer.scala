package wacc

import parsley.Parsley
import parsley.Parsley.atomic
import parsley.token.{Lexer, Basic}
import parsley.token.descriptions.*

import parsley.character.{string}
// import lexer.implicits.implicitSymbol

object lexer {

    private val desc = LexicalDesc.plain.copy(
        nameDesc = NameDesc.plain.copy(
            identifierStart = Basic(idStart),
            identifierLetter = Basic(idEnd),
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
            characterLiteralEnd = '\'',
            stringEnds = sEnds,
            escapeSequences = EscapeDesc.plain.copy(
                escBegin = '\\',
                literals = escapedLiterals,
                mapping = escapedMapping
            ),
            graphicCharacter = Basic(c => !(escapedLiterals.contains(c))),
        )
    )

    private val lexer = Lexer(desc)

    val _int: Parsley[BigInt] = lexer.lexeme.integer.decimal32
    val _ident: Parsley[String] = lexer.lexeme.names.identifier
    val _char: Parsley[Char] = lexer.lexeme.character.ascii
    val _string: Parsley[String] = lexer.lexeme.string.ascii
    val _bool: Parsley[Boolean] = lexer.lexeme(atomic((string("true") as true) | (string("false") as false)))
    val implicits = lexer.lexeme.symbol.implicits
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)

    def idStart(c: Char): Boolean = c.isLetter
    def idEnd(c: Char): Boolean = c.isLetterOrDigit
}