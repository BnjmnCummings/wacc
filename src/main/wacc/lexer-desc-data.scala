package wacc

val escapedMapping = Map(
                    "0" -> 0x00, 
                    "b" -> 0x08,
                    "t" -> 0x09,
                    "n" -> 0x0a,
                    "f" -> 0x0c,
                    "r" -> 0x0d,
                )

val hardKwdsSet = Set("null", "if", "then", "else", "fi", "while", 
                "do", "done", "newpair", "fst", "snd", 
                "read", "free", "return", "exit", "print", "call",
                "println", "int", "bool", "char", "string", "pair", 
                "begin", "end", "skip", "len", "ord", "chr")

val escapedLiterals = Set('\"','\'','\\')

val commentStart = "#"

val charEnd = '\''

val escChar = '\\'

val sEnds = Set(("\"", "\""))