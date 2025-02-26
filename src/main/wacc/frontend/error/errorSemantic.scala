package wacc

import java.io.File
import scala.io.Source

trait semanticErr

object ScopeError {
    def apply(msg: String)(using ctx: RenamerContext) = Err(
        ctx.fname,
        ctx.pos,
        SpecializedError(
            Set(msg),
            getLineFromContext(ctx)
        ),
        ErrorType.SemanticError
    )
}

object TypeMismatch {
    def apply(unexpected: SemType, expected: SemType)(using ctx: TypeCheckerCtx) = Err(
        ctx.fname,
        ctx.pos,
        VanillaError(
            Some(toErrorItem(unexpected)),
            Set(toErrorItem(expected)),
            Set(),
            getLineFromContext(ctx)
        ),
        ErrorType.SemanticError
    )
}

object NonExitableType {
    def apply(unexpected: SemType)(using ctx: TypeCheckerCtx) = Err(
        ctx.fname,
        ctx.pos,
        VanillaError(
            Some(toErrorItem(unexpected)),
            Set(toErrorItem(KnownType.Int)),
            Set(s"${typeToString(unexpected)} is not a valid exit code type"),
            getLineFromContext(ctx)
        ),
        ErrorType.SemanticError
    )
}

object NonFreeableType {
    def apply(unexpected: SemType)(using ctx: TypeCheckerCtx) = Err(
        ctx.fname,
        ctx.pos,
        VanillaError(
            Some(toErrorItem(unexpected)),
            Set(toErrorItem(KnownType.Array(?)), toErrorItem(KnownType.Pair(?, ?))),
            Set(s"can't free objects of type ${typeToString(unexpected)}"),
            getLineFromContext(ctx)
        ),
        ErrorType.SemanticError
    )
}

object NonNumericType {
    def apply(unexpected: SemType)(using ctx: TypeCheckerCtx) = Err(
        ctx.fname,
        ctx.pos,
        VanillaError(
            Some(toErrorItem(unexpected)),
            Set(toErrorItem(KnownType.Int)),
            Set(s"${typeToString(unexpected)} is not a numeric type"),
            getLineFromContext(ctx)
        ),
        ErrorType.SemanticError
    )
}

object NonCharacterType {
    def apply(unexpected: SemType)(using ctx: TypeCheckerCtx) = Err(
        ctx.fname,
        ctx.pos,
        VanillaError(
            Some(toErrorItem(unexpected)),
            Set(toErrorItem(KnownType.Char)),
            Set(s"${typeToString(unexpected)} is not a character type"),
            getLineFromContext(ctx)
        ),
        ErrorType.SemanticError
    )
}

object NonNumericCharacterType {
    def apply(unexpected: SemType)(using ctx: TypeCheckerCtx) = Err(
        ctx.fname,
        ctx.pos,
        VanillaError(
            Some(toErrorItem(unexpected)),
            Set(toErrorItem(KnownType.Char),toErrorItem(KnownType.Int)),
            Set(s"${typeToString(unexpected)} is not a character or numeric type"),
            getLineFromContext(ctx)
        ),
        ErrorType.SemanticError
    )
}

object NonBooleanType {
    def apply(unexpected: SemType)(using ctx: TypeCheckerCtx) = Err(
        ctx.fname,
        ctx.pos,
        VanillaError(
            Some(toErrorItem(unexpected)),
            Set(toErrorItem(KnownType.Boolean)),
            Set(s"${typeToString(unexpected)} is not a boolean"),
            getLineFromContext(ctx)
        ),
        ErrorType.SemanticError
    )
}

object NonStringType {
    def apply(unexpected: SemType)(using ctx: TypeCheckerCtx) = Err(
        ctx.fname,
        ctx.pos,
        VanillaError(
            Some(toErrorItem(unexpected)),
            Set(toErrorItem(KnownType.String)),
            Set(s"${typeToString(unexpected)} is not a string"),
            getLineFromContext(ctx)
        ),
        ErrorType.SemanticError
    )
}

object NonReadableType {
    def apply(unexpected: SemType)(using ctx: TypeCheckerCtx) = Err(
        ctx.fname,
        ctx.pos,
        VanillaError(
            Some(toErrorItem(unexpected)),
            Set(toErrorItem(KnownType.Int), toErrorItem(KnownType.Char)),
            Set(s"can\'t read items of type ${typeToString(unexpected)}"),
            getLineFromContext(ctx)
        ),
        ErrorType.SemanticError
    )
}

object InvalidReturn {
    def apply()(using ctx: TypeCheckerCtx) = Err(
        ctx.fname,
        ctx.pos,
        SpecializedError(
            Set("return is not allowed outside of a function body"),
            getLineFromContext(ctx)
        ),
        ErrorType.SemanticError
    )
}

object UnknownType {
    def apply(pTypeIn: SemType)(using ctx: TypeCheckerCtx) = Err(
        ctx.fname,
        ctx.pos,
        SpecializedError(
            Set("Can't infer type of pair"),
            getLineFromContext(ctx)
        ),
        ErrorType.SemanticError
    )
}

object WrongNumberOfArgs {
    def apply(unexpected: Int, expected: Int)(using ctx: TypeCheckerCtx) = Err(
        ctx.fname,
        ctx.pos,
        SpecializedError(
            Set(s"Wrong number of arguments, got $unexpected, expected $expected"),
            getLineFromContext(ctx)
        ),
        ErrorType.SemanticError
    )
}

object InvalidIndexing {
    def apply()(using ctx: TypeCheckerCtx) = Err(
        ctx.fname,
        ctx.pos,
        SpecializedError(
            Set("Invalid array indexing"),
            getLineFromContext(ctx)
        ),
        ErrorType.SemanticError
    )
}

def getLineFromContext(ctx: ErrContext): String = {
    ctx.fname match
        case Some(filename) => {
            val f: File = File(filename)
            val contents: Iterator[String] = Source.fromFile(f).getLines()
            var lineBefore: String = ""
            if (ctx.pos._1 > 1) {
                lineBefore = contents.drop(ctx.pos._1 - 2).next()
            }

            val curLine: String = contents.next()
            var lineAfter: String = ""
            if (contents.hasNext) {
                lineAfter = contents.next()
            }

            genErrorMessageCodeBlock(
                curLine,
                List(lineBefore),
                List(lineAfter),
                ctx.pos._2 - 1,
                1
                )
        }
        case None => "give us a file idiot"
    }

def toErrorItem(t: SemType): ErrorItem = NamedItem(typeToString(t))

def typeToString(t: SemType): String = t match
    case KnownType.Ident => "identifier"
    case KnownType.Int => "integer"
    case KnownType.Boolean => "boolean"
    case KnownType.String => "string"
    case KnownType.Char => "char"
    case KnownType.Array(?) => "array"
    case KnownType.Pair(?, ?) => "pair"
    case KnownType.Array(t) => s"array of ${typeToString(t)}"
    case KnownType.Pair(t1, t2) => s"pair of ${typeToString(t1)} and ${typeToString(t2)}"
    case X => "confused type (oh dear)"
    case ? => "any"
