package wacc.error

import wacc.semantic.*
import java.io.File
import scala.io.Source

trait semanticErr

/**
  * An object to represent a Scope Error.
  * @example A variable is accessed outside of it's scope.
  */
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

/**
  * An object to represent a Type Mismatch Error.
  * @example An assignment is made between a boolean and an integer
  */
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

/**
  * An object to represent a Non Exitable Error.
  * @example An exit() call is made with a string argument
  */
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

/**
  * An object to represent a Non Freeable Error.
  * @example A free() call is made for a stack-allocated variable.
  */
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

/**
  * An object to represent a Non Numeric Type Error.
  * @example A numerical expression written with boolean/string types
  */
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

/**
  * An object to represent a Non Character Type Error.
  * @example An 'ord' operator is invoked on an integer.
  */
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

/**
  * An object to represent a Non Numeric nor Character Type Error.
  * @example A read() call is made with a boolean argument
  */
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

/**
  * An object to represent a Non Boolean Type Error.
  * @example An if condition evaluates to be an integer.
  */
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

/**
  * An object to represent a Non String Type Error.
  */
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

/**
  * An object to represent a Non Readable Type Error.
  * @example a read() is invoked on a Boolean type.
  */
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

/**
  * An object to represent an Invalid Return Error.
  * @example a 'return' statement is called outside of a function.
  */
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

/**
  * An object to represent an Wrong Number of Arguments Error.
  * @example a function is called on the wrong number of arguments.
  */
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

/**
  * An object to represent an Invalid Index Error.
  * @example a 2d array is queried with 4 nested indicies.
  */
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

/**
  * Extracts the error line from the context.
  * @param ctx the [[ErrContext]], representing the current state of the semantic checking process.
  * @return the line where the error occurred as a string.
  */
def getLineFromContext(ctx: ErrContext): String = ctx.fname match
    case Some(filename) => 
        val f: File = File(filename)
        val contents: Iterator[String] = Source.fromFile(f).getLines()
        var lineBefore: String = ""
        if (ctx.pos._1 > 1) 
            lineBefore = contents.drop(ctx.pos._1 - 2).next()

        val curLine: String = contents.next()
        var lineAfter: String = ""
        if (contents.hasNext) 
            lineAfter = contents.next()

        genErrorMessageCodeBlock(
            curLine,
            List(lineBefore),
            List(lineAfter),
            ctx.pos._2 - 1,
            1
        )
        
    case None => "give us a file boy"
    
/**
  * Helper function that wraps up types into error items.
  * @param t the type causing an error.
  * @return a [[NamedItem]] representing the type.
  */
private def toErrorItem(t: SemType): ErrorItem = NamedItem(typeToString(t))

/**
  * Helper function that stringifies [[SemType]] objects.
  * @param t the type.
  * @return a string representing that type
  */
private def typeToString(t: SemType): String = t match
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
