package wacc

import parsley.Parsley
import parsley.token.{Lexer, Basic}
import parsley.token.descriptions.*

object lexer {
    private val desc = LexicalDesc.plain.copy(
        nameDesc = NameDesc.plain.copy(
            identifierStart = Basic(_.isLetter),
            identifierLetter = Basic(_.isLetterOrDigit),
        ),
        spaceDesc = SpaceDesc.plain,
        symbolDesc = SymbolDesc.plain.copy(
            hardKeywords = Set("null", "if", "then", "else", "fi", "while", "do", "done", "for", "newpair", "fst", "snd", "read", "free", "return", "exit", "print", "println", "int", "bool", "char", "string", "pair", "array", "begin", "end", "isnull", "skip", "len", "ord", "chr"),
        ),
    )
    private val lexer = Lexer(desc)

    val _int: Parsley[BigInt] = lexer.lexeme.integer.decimal
    val _ident: Parsley[String] = lexer.lexeme.names.identifier
    val implicits = lexer.lexeme.symbol.implicits
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
