package wacc.semantic

import wacc.syntax.* 
import collection.mutable

case class TypedProg(funcs: List[TypedFunc], body: List[TypedStmt])
case class TypedFunc(t: Type, id: Ident, args: List[TypedParam], body: List[TypedStmt])
case class TypedParam(t: Type, id: Ident)

def typeCheck(prog@(funcs: List[Func], body: List): Prog, tyInfo: TypeInfo): Either[List[Error], TypedProg] = { // Note this List is non-empty. Import won't work
    given ctx: TypeCheckerCtx[List[Error]] = TypeCheckerCtx(tyInfo, List.newBuilder)
    val typedProg = prog.map(check) // prog is not a list of stmt?? CHECK THIS!

    ctx.errors.match {
        case err :: errs => Left(err :: errs)
        case Nil         => Right(typedProg)
    }
}

class TypeCheckerCtx[C](tyInfo: TypeInfo, errs: mutable.Builder[Error, C]) {
    def errors: C = errs.result()

    // This will get the type of variables
    def typeOf(id: String): KnownType = tyInfo.varTys(id)

    def error(err: Error) = {
        errs += err
        None
    }
}

// This will get the most specific type out of 2 given
// This means if we know something is an Int, we can type check using that rather than ?
def mostSpecific(ty1: Option[SemType], ty2: Option[SemType]): SemType = (ty1, ty2) match {
    case (Some(?), Some(t)) => t
    case (Some(t), _)       => t
    case (None, t)          => t.getOrElse(?)
}

class TypeInfo(
    var varTys: Map[String, KnownType],
    var funcTys: Map[String, KnownType, List[KnownType]] // Check with Aidan to see how this would work
)

enum Error {
    case TypeMismatch(actual: SemType, expected: SemType)
    case NonExitableType(actual: SemType)
    case NonFreeableType(actual: SemType)
    case NonNumericType(actual: SemType)
    case NonCharacterType(actual: SemType)
    case NonBooleanType(actual: SemType)
    case NonStringType(actual: SemType)
}

enum Constraint {
    case Is(refTy: SemType)
    case IsNumeric
    case IsCharacter
    case IsBoolean
    case IsExitable
    case IsFreeable
    case IsString
}

object Constraint {
    val Unconstrained = Is(?) // Always passes
    val IsArray = Is(KnownType.Array(?))
}