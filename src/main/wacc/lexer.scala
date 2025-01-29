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
            hardKeywords = HARD_KEYWORDS,
        ),
    )
    private val lexer = Lexer(desc)

    private val HARD_KEYWORDS = 
        Set("null", "if", "then", "else", "fi", "while", 
            "do", "done", "for", "newpair", "fst", "snd", 
            "read", "free", "return", "exit", "print", "call",
            "println", "int", "bool", "char", "string", "pair", 
            "begin", "end", "skip", "len", "ord", "chr")

    val _int: Parsley[BigInt] = lexer.lexeme.integer.decimal
    val _ident: Parsley[String] = lexer.lexeme.names.identifier
    val implicits = lexer.lexeme.symbol.implicits
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
